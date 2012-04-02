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
use Formula;
use Utils qw(ensure_readable_file slurp);
use Data::Dumper;

Readonly my $TPTP4X => 'tptp4X';
Readonly my $EMPTY_STRING => q{};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };

has 'path' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_path',
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

sub BUILD {
    my $self = shift;

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

    my $path = $self->get_path ();
    my %predicate_symbol_arities = ();
    my %function_symbol_arities = ();
    my %function_symbol_table = ();
    my %predicate_symbol_table = ();

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
                                                   ([a-zA-Z0-9_]+)
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

    $self->_set_function_symbol_table (\%function_symbol_table);
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
    my @axioms = $self->get_axioms ();

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

    my $path = $self->get_path ();
    my @formulas = $self->get_formulas (1);

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

    my $path = $self->get_path ();
    my @formulas = $self->get_formulas (1);

    (my $new_fh, my $new_path) = tempfile ();

    foreach my $formula (@formulas) {
	my $formula_name = $formula->get_name ();
	if (any { $formula_name eq $_ } @formulas_to_remove) {
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

sub postulate {
    my $self = shift;
    my $new_axioms_ref = shift;

    my @new_axioms = @{$new_axioms_ref};

    my $theory = $self;

    # warn 'New axioms:', $SPACE, Dumper (@new_axioms);

    foreach my $formula (@new_axioms) {
	my $axiom = $formula->make_axiom ();
	$theory = $theory->add_formula ($axiom);
    }

    return $theory;

}

1;
__END__
