package Theory;

use Moose;
use IPC::Cmd qw(can_run);
use IPC::Run qw(run start timer harness);
use Carp qw(croak confess carp);
use Readonly;
use charnames qw(:full);
use List::MoreUtils qw(firstidx any);
use File::Temp qw(tempfile);
use Regexp::DefaultFlags;
use Utils qw(ensure_readable_file slurp);
use Data::Dumper;

# Our modules
use Formula;
use SZS qw(szs_implies is_szs_success szs_contradicts);

Readonly my $TPTP4X => 'tptp4X';
Readonly my $EMPTY_STRING => q{};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $COLON => q{:};
Readonly my $FULL_STOP => q{.};
Readonly my $DEBUG => q{DEBUG};

has 'path' => (
    is => 'rw',
    isa => 'Str',
    reader => 'get_path',
    writer => '_set_path',
    required => 1,
);

has 'conjecture' => (
    isa => 'Formula',
    is => 'rw',
    writer => '_set_conjecture',
);

has 'formula_table' => (
    isa => 'HashRef',
    is => 'rw',
    writer => '_set_formula_table',
    reader => 'get_formula_table',
);

has function_symbol_table => (
    isa => 'HashRef',
    is => 'rw',
    writer => '_set_function_symbol_table',
    reader => 'get_function_symbol_table',
);

has predicate_symbol_table => (
    isa => 'HashRef',
    is => 'rw',
    writer => '_set_predicate_symbol_table',
    reader => 'get_predicate_symbol_table',
);

has function_symbol_arities => (
    isa => 'HashRef',
    is => 'rw',
    writer => '_set_function_symbol_arities',
    reader => 'get_function_symbol_arities',
);

has predicate_symbol_arities => (
    isa => 'HashRef',
    is => 'rw',
    writer => '_set_predicate_symbol_arities',
    reader => 'get_predicate_symbol_arities',
);

sub BUILD {
    my $self = shift;
    my $path = $self->get_path ();

    if ($path eq '--') {
	my $theory_content = slurp *STDIN
	    or croak 'Unable to slurp the theory from standard input.';
	(my $new_fh, my $new_path) = tempfile ();
	say {$new_fh} $theory_content
	    or croak 'Unable to print a copy of the theory on standard input to a temporary filehandle.';
	close $new_fh
	    or croak 'Unable to close the output filehandle for the temporary file containing the theory we just read from standard input.';
	$self->_set_path ($new_path);
	$path = $new_path;
    }

    my %formula_table = ();

    my @formulas = $self->get_formulas (1);

    foreach my $formula (@formulas) {
	my $name = $formula->get_name ();
	my $status = $formula->get_status ();
	$formula_table{$name} = $formula;
	if ($status eq 'conjecture') {
	    $self->_set_conjecture ($formula);
	}
    }

    my %predicate_symbol_arities = ();
    my %function_symbol_arities = ();
    my %function_symbol_table = ();
    my %predicate_symbol_table = ();

    if (scalar @formulas > 0) {

	my @GetSymbols_call = ('GetSymbols', $path);
	my $GetSymbols_out = $EMPTY_STRING;
	my $GetSymbols_err = $EMPTY_STRING;
	my $GetSymbols_harness = harness (\@GetSymbols_call,
					  '>', \$GetSymbols_out,
					  '2>', \$GetSymbols_err);

	$GetSymbols_harness->run ();
	$GetSymbols_harness->finish ();

	my @GetSymbols_harness_results = $GetSymbols_harness->results ();

	if (scalar @GetSymbols_harness_results == 0) {
	    if ($GetSymbols_err eq $EMPTY_STRING) {
		confess 'Something went badly wrong calling GetSymbols (did it crash?).  We did not even get any error output.';
	    } else {
		confess 'Something went badly wrong calling GetSymbols (did it crash?).  Here is its error output: ', $GetSymbols_err;
	    }
	}

	my $GetSymbols_exit_code = $GetSymbols_harness_results[0];

	if ($GetSymbols_exit_code != 0) {
	    if ($GetSymbols_err eq $EMPTY_STRING) {
		confess 'Something went wrong calling GetSymbols on', "\N{LF}", "\N{LF}", $TWO_SPACES, $path, "\N{LF}", "\N{LF}", 'Its exit code was', $SPACE, $GetSymbols_exit_code, '.  Somehow, it did not produce any error output.'
	    } else {
		confess 'Something went wrong calling GetSymbols; its exit code was', $SPACE, $GetSymbols_exit_code, '.  The error output was:', "\N{LF}", $GetSymbols_err, "\N{LF}";
	    }
	}

	my @symbols_by_formula = split ("\N{LF}", $GetSymbols_out);

	foreach my $symbol_by_formula (@symbols_by_formula) {
	    if ($symbol_by_formula =~ / \A symbols [(]
                                                   (.+)
                                                   [,]
                                                   [[] (.*) []]
                                                   [,]
                                                   [[] (.*) []]
                                               [)] [.] \z/) {
		(my $formula_name, my $function_symbols_str, my $predicate_symbols_str) = ($1, $2, $3);

		my @function_symbols = split (',', $function_symbols_str);
		my @predicate_symbols = split (',', $predicate_symbols_str);

		foreach my $function_symbol_info (@function_symbols) {
		    if ($function_symbol_info =~ / \A (.+) [\/] ([0-9]+) [\/] ([0-9]+) \z/) {
			(my $name, my $arity, my $num_occurrences) = ($1, $2, $3);
			$function_symbol_arities{$name} = $arity;
			$function_symbol_table{$formula_name}{$name} = $num_occurrences;
		    } else {
			croak 'Unable to make sense of the part', "\N{LF}", "\N{LF}", $TWO_SPACES, $function_symbol_info, "\N{LF}", "\N{LF}", 'emitted by GetSymbols';
		    }
		}

		foreach my $predicate_symbol_info (@predicate_symbols) {
		    if ($predicate_symbol_info =~ / \A (.+) [\/] ([0-9]+) [\/] ([0-9]+) \z/) {
			(my $name, my $arity, my $num_occurrences) = ($1, $2, $3);
			$predicate_symbol_arities{$name} = $arity;
			$predicate_symbol_table{$formula_name}{$name} = $num_occurrences;
		    } else {
			croak 'Unable to make sense of the part', "\N{LF}", "\N{LF}", $TWO_SPACES, $predicate_symbol_info, "\N{LF}", "\N{LF}", 'emitted by GetSymbols';
		    }
		}

	    } else {
		croak 'Unable to make sense of the GetSymbols line', "\N{LF}", "\N{LF}", $symbol_by_formula;
	    }
	}
    }

    $self->_set_function_symbol_arities (\%function_symbol_arities);
    $self->_set_function_symbol_table (\%function_symbol_table);
    $self->_set_predicate_symbol_arities (\%predicate_symbol_arities);
    $self->_set_predicate_symbol_table (\%predicate_symbol_table);

    $self->_set_formula_table (\%formula_table);

}

sub content {
    my $self = shift;
    my $path = $self->get_path ();
    return slurp ($path);
}

sub formula_with_name {
    my $self = shift;
    my $name = shift;

    my %formula_table = %{$self->get_formula_table ()};

    if (defined $formula_table{$name}) {
	return $formula_table{$name};
    } else {
	my $path = $self->get_path ();
	croak 'No such formula called \'', $name, '\' in the TPTP file ', $path, '.';
    }
}

sub get_formulas {
    my $self = shift;
    my $expand_includes = shift;

    my $path = $self->get_path ();

    my @tptp4x_call = ('tptp4X', '-N', '-V', '-c', '-umachine');

    if (defined $expand_includes && $expand_includes) {
	push (@tptp4x_call, '-x');
    }

    push (@tptp4x_call, $path);

    my $tptp4x_err = $EMPTY_STRING;
    my $tptp4x_out = $EMPTY_STRING;

    my $tptp4x_harness;

    if (defined $expand_includes && $expand_includes) {
	$tptp4x_harness = harness (\@tptp4x_call,
				   '>', \$tptp4x_out,
				   '2>', \$tptp4x_err);
    } else {
	my @grep_call = ('grep', '--invert-match', '^include(');
	$tptp4x_harness = harness (\@tptp4x_call,
				   '|',
				   \@grep_call,
				   '>', \$tptp4x_out,
				   '2>', \$tptp4x_err);

    }

    $tptp4x_harness->start ();
    $tptp4x_harness->finish ();

    my $tptp4x_exit_code = $tptp4x_harness->result (0);

    if ($tptp4x_exit_code != 0) {
	if ($tptp4x_err eq $EMPTY_STRING) {
	    croak 'tptp4X did not exit cleanly working on ', $path, '; there was no error output.';
	} else {
	    croak 'tptp4X did not exit cleanly working on ', $path, '; the error output was:', "\n", $tptp4x_err;
	}
    }

    my @formula_strings = split ("\n", $tptp4x_out);

    my @formulas = map { Formula::make_formula ($_) } @formula_strings;

    if (wantarray) {
	return @formulas;
    } else {
	return \@formulas;
    }

}

sub get_conjecture {
    my $self = shift;
    my @formulas = $self->get_formulas ();
    my $conjecture = undef;
  CONJECTURE:
    foreach my $formula (@formulas) {
	my $status = $formula->get_status ();
	if ($status eq 'conjecture') {
	    $conjecture = $formula;
	    last CONJECTURE;
	}
    }

    if (defined $conjecture) {
	return $conjecture;
    } else {
	my $path = $self->get_path ();
	croak 'We did not find a conjecture formula in the TPTP file at ', $path, '.';
    }
}

sub get_axioms {
    my $self = shift;
    my $expand_includes = shift;

    my @formulas = $self->get_formulas ($expand_includes);

    my @axioms = ();
    foreach my $formula (@formulas) {
	my $status = $formula->get_status ();
	if ($status ne 'conjecture') {
	    push (@axioms, $formula);
	}
    }

    if (wantarray) {
	return @axioms;
    } else {
	return \@axioms;
    }

}

sub has_conjecture_formula {
    my $self = shift;
    my @formulas = $self->get_formulas (1);
    my $conjecture_formula = undef;
    my $conjecture_idx = firstidx { $_->get_status () eq 'conjecture' } @formulas;

    return ($conjecture_idx >= 0);

}

sub strip_conjecture {
    my $self = shift;

    my $path = $self->get_path ();
    my @axioms = $self->get_axioms (1);

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $axiom (@axioms) {
	print {$new_fh} $axiom->tptpify (), "\N{LF}";
    }

    close $new_fh
	or croak 'Error: unable to close the output filehandle for the conjecture-free variant of ', $path, '.';

    return Theory->new (path => $new_path);

}

sub promote_conjecture_to_true_axiom {
    my $self = shift;

    my $path = $self->get_path ();
    my @axioms = $self->get_axioms (1);

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $axiom (@axioms) {
	print {$new_fh} $axiom->tptpify (), "\N{LF}";
    }

    if ($self->has_conjecture_formula ()) {
	my $conjecture = $self->get_conjecture ();
	my $conjecture_as_axiom = $conjecture->change_status ('axiom');
	print {$new_fh} $conjecture_as_axiom->tptpify (), "\N{LF}";
    }

    close $new_fh
	or croak 'Error: unable to close the output filehandle for the conjecture-free variant of ', $path, '.';

    return Theory->new (path => $new_path);
}

sub promote_conjecture_to_false_axiom {
    my $self = shift;

    my $path = $self->get_path ();
    my @axioms = $self->get_axioms (1);

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $axiom (@axioms) {
	print {$new_fh} $axiom->tptpify (), "\N{LF}";
    }

    if ($self->has_conjecture_formula ()) {
	my $conjecture = $self->get_conjecture ();
	my $negated_conjecture = $conjecture->negate ();
	my $conjecture_as_axiom = $negated_conjecture->change_status ('axiom');
	print {$new_fh} $conjecture_as_axiom->tptpify (), "\N{LF}";
    }

    close $new_fh
	or croak 'Error: unable to close the output filehandle for the conjecture-free variant of ', $path, '.';

    return Theory->new (path => $new_path);
}

sub remove_formula {
    my $self = shift;
    my $formula_to_remove = shift;
    my $name_of_formula_to_remove = $formula_to_remove->get_name ();
    return $self->remove_formula_by_name ($name_of_formula_to_remove);
}

sub remove_formula_by_name {
    my $self = shift;
    my $name_of_formula_to_remove = shift;

    my @formulas = $self->get_formulas (1);
    my $path = $self->get_path ();

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $formula (@formulas) {
	my $formula_name = $formula->get_name ();
	if ($formula_name ne $name_of_formula_to_remove) {
	    print {$new_fh} $formula->tptpify (), "\N{LF}";
	}
    }

    close $new_fh
	or croak 'Error: unable to close the output filehandle for the conjecture-free variant of ', $path, '.';

    return Theory->new (path => $new_path);

}

sub remove_formulas {
    my $self = shift;
    my @formulas_to_remove = @_;
    my @names_of_formulas_to_remove = map { $_->get_name () } @formulas_to_remove;
    return $self->remove_formulas_by_name (@names_of_formulas_to_remove);
}

sub remove_formulas_by_name {
    my $self = shift;
    my @names_of_formulas_to_remove = @_;

    my $path = $self->get_path ();
    my @formulas = $self->get_formulas (1);
    (my $new_fh, my $new_path) = tempfile ();

    foreach my $formula (@formulas) {
	my $formula_name = $formula->get_name ();
	if (any { $formula_name eq $_ } @names_of_formulas_to_remove) {
	    # do nothing
	} else {
	    print {$new_fh} $formula->tptpify (), "\N{LF}";
	}
    }

    close $new_fh
	or croak 'Error: unable to close the output filehandle for the conjecture-free variant of ', $path, '.';

    return Theory->new (path => $new_path);

}

sub add_formula {
    my $self = shift;
    my $formula = shift;

    my $path = $self->get_path ();
    my @axioms = $self->get_axioms (1);

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	print {$new_fh} $axiom->tptpify (), "\N{LF}";
    }

    if ($self->has_conjecture_formula ()) {
	my $conjecture = $self->get_conjecture ();
	print {$new_fh} $conjecture->tptpify (), "\N{LF}";
    }

    print {$new_fh} $formula->tptpify (), "\N{LF}";

    close $new_fh
	or croak 'Error: unable to close the output filehandle for ', $path, '.';

    return Theory->new (path => $new_path);

}

sub tptpify {
    my $self = shift;

    my $tptp_form = $EMPTY_STRING;

    my @formulas = $self->get_formulas (1);

    foreach my $formula (@formulas) {
	$tptp_form .= $formula->tptpify ();
	$tptp_form .= "\N{LF}";
    }

    return $tptp_form;
}

sub get_function_symbol_counts {
    my $self = shift;

    my %function_table = %{$self->get_function_symbol_table ()};
    my %function_symbol_counts = ();

    foreach my $formula (keys %function_table) {

	my %by_formula = %{$function_table{$formula}};
	foreach my $symbol (keys %by_formula) {
	    my $count_for_this_formula = $by_formula{$symbol};
	    if (defined $function_symbol_counts{$symbol}) {
		$function_symbol_counts{$symbol} += $count_for_this_formula;
	    } else {
		$function_symbol_counts{$symbol} = $count_for_this_formula;
	    }
	}

    }

    return \%function_symbol_counts;
}

sub get_predicate_symbol_counts {
    my $self = shift;

    my %predicate_table = %{$self->get_predicate_symbol_table ()};
    my %predicate_symbol_counts = ();

    foreach my $formula (keys %predicate_table) {

	my %by_formula = %{$predicate_table{$formula}};
	foreach my $symbol (keys %by_formula) {
	    my $count_for_this_formula = $by_formula{$symbol};
	    if (defined $predicate_symbol_counts{$symbol}) {
		$predicate_symbol_counts{$symbol} += $count_for_this_formula;
	    } else {
		$predicate_symbol_counts{$symbol} = $count_for_this_formula;
	    }
	}

    }

    return \%predicate_symbol_counts;
}

sub get_function_symbols {
    my $self = shift;

    my %function_symbol_table = %{$self->get_function_symbol_table ()};
    my %function_symbols = ();

    foreach my $formula (keys %function_symbol_table) {
	my %function_symbols_for_formula = %{$function_symbol_table{$formula}};
	foreach my $symbol (keys %function_symbols_for_formula) {
	    $function_symbols{$symbol} = 0;
	}
    }

    my @function_symbols = keys %function_symbols;

    if (wantarray) {
	return @function_symbols;
    } else {
	return \@function_symbols;
    }

}

sub get_predicate_symbols {
    my $self = shift;

    my %predicate_symbol_table = %{$self->get_predicate_symbol_table ()};
    my %predicate_symbols = ();

    foreach my $formula (keys %predicate_symbol_table) {
	my %predicate_symbols_for_formula = %{$predicate_symbol_table{$formula}};
	foreach my $symbol (keys %predicate_symbols_for_formula) {
	    $predicate_symbols{$symbol} = 0;
	}
    }

    my @predicate_symbols = keys %predicate_symbols;

    if (wantarray) {
	return @predicate_symbols;
    } else {
	return \@predicate_symbols;
    }

}

sub get_all_symbols {
    my $self = shift;

    my @function_symbols = $self->get_function_symbols ();
    my @predicate_symbols = $self->get_predicate_symbols ();

    my @all_symbols = ();
    push (@all_symbols, @function_symbols);
    push (@all_symbols, @predicate_symbols);

    if (wantarray) {
	return @all_symbols;
    } else {
	return \@all_symbols;
    }

}

sub has_function_symbol {
    my $self = shift;
    my $symbol = shift;

    my @function_symbols = @{$self->get_function_symbols ()};

    return any { $_ eq $symbol } @function_symbols;
}

sub has_predicate_symbol {
    my $self = shift;
    my $symbol = shift;

    my @predicate_symbols = @{$self->get_predicate_symbols ()};

    return any { $_ eq $symbol } @predicate_symbols;
}

sub has_symbol {
    my $self = shift;
    my $symbol = shift;

    return ($self->has_function_symbol ($symbol)
		|| $self->has_predicate_symbol ($symbol));
}

sub arity_of_symbol {
    my $self = shift;
    my $symbol = shift;

    my $path = $self->get_path ();

    if ($self->has_predicate_symbol ($symbol)) {
	my %predicate_symbol_arities = %{$self->get_predicate_symbol_arities ()};
	if (defined $predicate_symbol_arities{$symbol}) {
	    return $predicate_symbol_arities{$symbol};
	} else {
	    croak 'The theory at', $SPACE, $path, $SPACE, 'has the symbol', $SPACE, $symbol, $SPACE, ', but it is missing from the predicate symbol arities table.';
	}
    } elsif ($self->has_function_symbol ($symbol)) {
	my %function_symbol_arities = %{$self->get_function_symbol_arities ()};
	if (defined $function_symbol_arities{$symbol}) {
	    return $function_symbol_arities{$symbol};
	} else {
	    croak 'The theory at', $SPACE, $path, $SPACE, 'has the symbol', $SPACE, $symbol, $SPACE, ', but it is missing from the function symbol arities table.';
	}
    } else {
	croak 'The symbol', $SPACE, $symbol, $SPACE, 'is not used by any formula of the theory at', $SPACE, $path, $SPACE, $FULL_STOP;
    }
}

sub postulate {
    my $self = shift;
    my $new_axioms_ref = shift;

    my @new_axioms = @{$new_axioms_ref};

    my $path = $self->get_path ();
    my $theory = Theory->new (path => $path);

    # warn 'New axioms:', $SPACE, Dumper (@new_axioms);

    foreach my $formula (@new_axioms) {
	my $axiom = $formula->make_axiom ();
	$theory = $theory->add_formula ($axiom);
    }

    return $theory;

}

sub independent_axiom {
    my $self = shift;
    my $axiom = shift;

    my $axiom_name = $axiom->get_name ();

    if (! $self->has_axiom ($axiom)) {
	my $axiom_as_tptp_formula = $axiom->tptpify ();
	my $path = $self->get_path ();
	croak 'The theory at', $SPACE, $path, $SPACE, 'has no axiom called', $SPACE, $axiom_name, $SPACE, 'or, if it does, it is not identical to the given axiom', "\N{LF}", "\N{LF}", $TWO_SPACES, $axiom_as_tptp_formula;
    }

    my $trimmed_theory = $self->remove_formula ($axiom);

    my $trimmed_theory_conjecture_false
	= $trimmed_theory->promote_conjecture_to_false_axiom ();

    # carp 'Testing independence of', $SPACE, $axiom_name, ', the trimmed theory is now:', "\N{LF}", Dumper ($trimmed_theory);

    my $satisfiable = $trimmed_theory_conjecture_false->is_satisfiable ();

    if ($satisfiable == -1) {
	# carp 'Model finder failed; trying a theorem prover.';
	my $proves = $trimmed_theory->proves ($axiom);
	if ($proves == -1) {
	    return -1;
	} elsif ($proves == 0) {
	    return 1;
	} else {
	    return 0;
	}
    } else {
	return $satisfiable;
    }

    return $trimmed_theory->is_satisfiable ();

}

sub is_satisfiable {
    my $self = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();
    my $model_result = TPTP::find_model ($self, \%parameters);

    my $model_szs_status
	= $model_result->has_szs_status () ? $model_result->get_szs_status () : 'Unknown';

    my $model_output = $model_result->get_output ();

    # carp 'Model output:', "\N{LF}", $model_output;

    if ($model_result->timed_out () || $model_szs_status eq 'Unknown') {
	# Try to use a theorem prover

	# carp 'Failed to determine satisfiability using a model finder (SZS status ', $model_szs_status, '); going for a theorem prover';

	my $prover_result = TPTP::prove ($self, 'eprover', \%parameters);

	# carp 'Prover result:', Dumper ($prover_result);

	my $prover_szs_status = $prover_result->has_szs_status () ? $prover_result->get_szs_status () : 'Unknown';
	if ($prover_result->timed_out ()) {
	    return -1;
	} elsif ($prover_szs_status eq 'Unknown') {
	    return -1;
	} elsif ($prover_szs_status eq 'Satisfiable') {
	    return 1;
	} elsif ($prover_szs_status eq 'Unsatisfiable') {
	    return 0;
	} else {
	    # Can't figure this out
	    return -1;
	}
    } elsif ($model_szs_status eq 'Unsatisfiable') {
	return 0;
    } elsif ($model_szs_status eq 'Satisfiable') {
	return 1;
    } else {
	# Can't figure this out
	return -1;
    }

}

sub solve {
    my $self = shift;
    my $prover = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $result = TPTP::prove ($self, $prover, \%parameters);

    if (defined $parameters{'debug'} && $parameters{'debug'}) {
	carp 'Result of calling ', $prover, $COLON, "\N{LF}", Dumper ($result);
    }

    return $result->get_szs_status ();
}

sub solvable_with {
    my $self = shift;
    my $prover = shift;
    my $intended_szs_status = shift;
    my $parameters_ref = shift;

    if (! defined $intended_szs_status) {
	confess 'An intended SZS status is required to know whether a theory is solvable.';
    }

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $szs_status = $self->solve ($prover, \%parameters);

    if (is_szs_success ($szs_status)) {
	return szs_implies ($szs_status, $intended_szs_status);
    } else {
	return 0;
    }

}

sub countersolvable_with {
    my $self = shift;
    my $prover = shift;
    my $intended_szs_status = shift;
    my $parameters_ref = shift;

    if (! defined $intended_szs_status) {
	confess 'An intended SZS status is required to know whether a theory is countersolvable.';
    }

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $szs_status = $self->solve ($prover, \%parameters);

    if (is_szs_success ($szs_status)) {
	return szs_contradicts ($szs_status, $intended_szs_status);
    } else {
	return 0;
    }

}

sub proves {
    my $self = shift;
    my $formula = shift;

    if ($self->has_conjecture_formula ()) {
	my $path = $self->get_path ();
	my $formula_name = $formula->get_name ();
	croak 'The theory at', "\N{LF}", "\N{LF}", $TWO_SPACES, $path, "\N{LF}", "\N{LF}", 'already has a conjecture formula; checking whether it proves', "\N{LF}", "\N{LF}", $TWO_SPACES, $formula_name, "\N{LF}", "\N{LF}", 'is not well-defined.';
    }

    my $formula_as_conjecture = $formula->make_conjecture ();
    my $new_theory = $self->add_formula ($formula_as_conjecture);

    my $result = TPTP::prove ($new_theory);
    my $szs_status
	= $result->has_szs_status () ? $result->get_szs_status () : 'Unknown';

    if ($result->timed_out ()) {
	return -1;
    } elsif ($szs_status eq 'Unknown') {
	return -1;
    } elsif ($szs_status eq 'Theorem') {
	return 1;
    } elsif ($szs_status eq 'Unsatisfiable') {
	return 1;
    } else {
	return 0;
    }

}

sub has_axiom {
    my $self = shift;
    my $maybe_axiom = shift;

    my $maybe_axiom_name = $maybe_axiom->get_name ();
    my @axioms = $self->get_axioms (1);
    my @axiom_names = map { $_->get_name () } @axioms;

    if (any { $_ eq $maybe_axiom_name } @axiom_names) {
	my $found_axiom = $self->formula_with_name ($maybe_axiom_name);
	return $maybe_axiom->equal_to ($found_axiom);
    } else {
	return 0;
    }

}

sub has_premise_with_name {
    my $self = shift;
    my $premise_name = shift;

    my @axioms = $self->get_axioms (1);
    my @axiom_names = map { $_->get_name () } @axioms;

    return (any { $_ eq $premise_name } @axiom_names);

}

sub fofify {
    my $self = shift;

    my $path = $self->get_path ();

    my @tptp4x_call
	= ('tptp4X', '-N', '-V', '-c', '-umachine', '-x', '-tfofify', $path);

    my $tptp4x_err = $EMPTY_STRING;
    my $tptp4x_out = $EMPTY_STRING;

    my $tptp4x_harness = harness (\@tptp4x_call,
				  '>', \$tptp4x_out,
				  '2>', \$tptp4x_err);

    $tptp4x_harness->start ();
    $tptp4x_harness->finish ();

    my $tptp4x_exit_code = $tptp4x_harness->result (0);

    if ($tptp4x_exit_code != 0) {
	if ($tptp4x_err eq $EMPTY_STRING) {
	    croak 'tptp4X did not exit cleanly working on ', $path, '; there was no error output.';
	} else {
	    croak 'tptp4X did not exit cleanly working on ', $path, '; the error output was:', "\n", $tptp4x_err;
	}
    }

    (my $new_fh, my $new_path) = tempfile ();

    print {$new_fh} $tptp4x_out
	or croak 'Unable to print the tptp4X fofify output to a temporary filehandle.';

    close $new_fh
	or croak 'Unable to close the output filehandle into which we are writing the fofify\'d version of', $SPACE, $path;

    return Theory->new (path => $new_path);

}

sub is_first_order {
    my $self = shift;

    my @formulas = $self->get_formulas (1);

    if (scalar @formulas == 0) {
	return 1;
    } else {
	if (any { ! $_->is_first_order () } @formulas) {
	    return 0;
	} else {
	    return 1;
	}
    }
}

sub copy {
    my $self = shift;
    my $path = $self->get_path ();
    return Theory->new (path => $path);
}

sub minimize {
    my $self = shift;
    my $prover = shift;
    my $intended_szs_status = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $theory_to_minimize = $self->copy ();

    my $result = TPTP::prove ($theory_to_minimize, $prover, \%parameters);
    my $last_known_good_result = undef;
    my $szs_status = $result->get_szs_status ();
    my $derivation = $result->output_as_derivation ();

    my @unused_premises = $derivation->get_unused_premises ();

    while (is_szs_success ($szs_status)
	       && szs_implies ($szs_status, $intended_szs_status)
		   && scalar @unused_premises > 0) {
	$last_known_good_result = $result;
	$theory_to_minimize = $derivation->theory_from_used_premises ();
	$result = TPTP::prove ($theory_to_minimize, $prover, \%parameters);
	$szs_status = $result->get_szs_status ();
	$derivation = is_szs_success ($szs_status) ? $result->output_as_derivation ()
	    : undef;
	@unused_premises = defined $derivation ? $derivation->get_unused_premises ()
	    : ();
    }

    return (is_szs_success ($szs_status) ? $result : $last_known_good_result);

}

1;
__END__
