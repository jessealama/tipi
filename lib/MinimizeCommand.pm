package MinimizeCommand;

require v5.10;

use Moose;
use Carp qw(croak carp);
use Pod::Usage;
use Readonly;
use Getopt::Long qw(GetOptionsFromArray :config gnu_compat);
use charnames qw(:full);
use English qw(-no_match_vars);
use Data::Dumper;
use Term::ANSIColor qw(colored);
use feature 'say';
use List::Util qw(max);
use List::MoreUtils qw(any none first_index all);
use Term::ProgressBar;
use Regexp::DefaultFlags;
use POSIX qw(ceil);
use Algorithm::Combinatorics qw(combinations);
use IPC::Cmd qw(can_run);
use IPC::Run qw(run start timer harness timeout);

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    known_prover
	    supported_provers
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    tptp4X_output
       );
use Utils qw(asterisk_list
	     error_message
	     warning_message
	     all_sublists
	     all_nonempty_sublists
	     remove_duplicate_lists
	     subtuple
	     tuple_less_than_wrt_ordering
	     ensure_readable_file);
use SZS qw(szs_camelword_for
	   is_szs_success
	   szs_implies);

Readonly my $EMPTY_STRING => q{};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $COMMA => q{,};
Readonly my $COLON => q{:};
Readonly my $SLASH => q{/};
Readonly my $ASTERISK => q{*};
Readonly my $LF => "\N{LF}";
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'yellow';
Readonly my $DESCRIPTION => 'Find minimal subtheories.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';
Readonly my $NEEDED_PREMISE_COLOR => 'red';
Readonly my $UNNEEDED_PREMISE_COLOR => 'cyan';

# Some useful SZS statuses
Readonly my $SZS_UNKNOWN => 'Unknown';
Readonly my $SZS_NOT_TRIED => 'NotTried';

my $opt_show_output = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_solution_szs_status = 'Theorem';
my @opt_provers = ();
my $opt_timeout = 30;
my $opt_skip_initial_proof = 0;
my $opt_show_only_final_used_premises = 0;
my $opt_show_only_final_unused_premises = 0;
my $opt_confirm = 0;

sub BUILD {
    my $self = shift;
    $self->_set_description ($DESCRIPTION);
    return $self;
}

sub fill_up_to_column {
    my $str = shift;
    my $column = shift;

    my $str_len = length $str;
    my $padding = $str_len < $column ? $SPACE x ($column - $str_len) : $EMPTY_STRING;
    return ($str . $padding);

}

sub print_formula_names_with_color {
    my $formulas_ref = shift;
    my $color = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my @formulas = @{$formulas_ref};

    if (defined $parameters{'sorted'} && $parameters{'sorted'}) {
	my @formula_names = map { $_->get_name () } @formulas;
	my @formula_names_sorted = sort @formula_names;
	my @formula_names_colored
	    = map { colored ($_, $color) } @formula_names_sorted;
	say join ("\N{LF}", @formula_names_colored);
    } else {
	my @formula_names_colored = map { $_->name_with_color ($color) } @formulas;
	say join ("\N{LF}", @formula_names_colored);
    }

    return 1;

}

around 'execute' => sub {
    my $orig = shift;
    my $self = shift;
    my @arguments = @_;

    GetOptionsFromArray (
	\@arguments,
	'man' => \$opt_man,
	'verbose' => \$opt_verbose,
	'help|?' => \$opt_help,
	'debug' => \$opt_debug,
	'with-prover=s' => \@opt_provers,
	'solution-szs-status=s' => \$opt_solution_szs_status,
	'timeout=i' => \$opt_timeout,
	'skip-initial-proof' => \$opt_skip_initial_proof,
	'confirm' => \$opt_confirm,
    ) or pod2usage (2);

    if ($opt_help) {
	pod2usage(1);
    }

    if ($opt_man) {
	pod2usage(
	    -exitstatus => 0,
	    -verbose    => 2
	);
    }

    # debug implies verbose
    if ($opt_debug) {
	$opt_verbose = 1;
    }

    if (scalar @arguments == 0) {
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2);
    }

    if (scalar @arguments > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
		   -exitval => 2);
    }

    if ($opt_solution_szs_status eq $EMPTY_STRING) {
	pod2usage (-msg => error_message ('The empty string is not an acceptable SZS problem status.'),
		   -exitval => 2);

    }

    if ($opt_solution_szs_status !~ /\A [A-Za-z]+ \z/) {
	pod2usage (-msg => error_message ('Unacceptable SZS problem status', "\N{LF}", "\N{LF}", $TWO_SPACES, $opt_solution_szs_status),
		   -exitval => 2);
    }

    if ($opt_show_only_final_used_premises && $opt_show_only_final_unused_premises) {
	pod2usage (-msg => error_message ('One cannot choose to show only the used and unused premises.'),
		   -exitval => 2);
    }

    if ($opt_timeout < 0) {
	pod2usage (-msg => error_message ('Invalid value ', $opt_timeout, ' for the --timeout option.'),
		   -exitval => 2);
    }

    if (scalar @opt_provers == 0) {
	@opt_provers = ('eprover', 'paradox');
    }

    my %provers = ();

    foreach my $tool (@opt_provers) {
	$provers{$tool} = 0;
    }

    @opt_provers = keys %provers;
    my @supported_provers = supported_provers ();

    foreach my $prover (@opt_provers) {
	if (known_prover ($prover)) {
	    if (can_run ($prover)) {
		next;
	    } else {
		say {*STDERR} error_message ($prover, $SPACE, 'is supported by tipi, but it could not be found.'), $LF;
		exit 1;
	    }
	} else {
	    say {*STDERR} error_message ('Unknown prover', $SPACE, $prover), $LF;
	    say {*STDERR} 'The following provers are known:', "\N{LF}";
	    if (scalar @supported_provers == 0) {
		say {*STDERR} '(none)';
	    } else {
		say asterisk_list (@supported_provers);
	    }
	    exit 1;
	}
    }

    if (! ensure_tptp4x_available ()) {
	say STDERR error_message ('Cannot run tptp4X');
	exit 1;
    }

    my $theory_path = $arguments[0];

    if (! ensure_readable_file ($theory_path)) {
	say STDERR error_message ('There is no file at', $SPACE, $theory_path, $SPACE, '(or it is unreadable).');
	exit 1;
    }

    if (! ensure_sensible_tptp_theory ($theory_path)) {
	my $errors = tptp4X_output ($theory_path);
	say {*STDERR} error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	say {*STDERR} 'Here is what tptp4X output when evaluating the file:';
	say {*STDERR} $errors;
	exit 1;
    }

    my $theory = Theory->new (path => $theory_path);

    if ($theory->is_first_order ()) {
	return $self->$orig (@arguments);
    } else {
	say {*STDERR} error_message ('The theory at', $SPACE, $theory_path, $SPACE, 'seems to be a non-first-order theory.');
    }

};

sub minimize {
    my $theory = shift;
    my $prover = shift;
    return $theory->minimize ($prover,
			      $opt_solution_szs_status,
			      { 'timeout' => $opt_timeout });
}

sub one_tool_solves {
    my $theory = shift;

    my %harnesses = ();
    my %parameters = ( 'timeout' => $opt_timeout );

    foreach my $prover (@opt_provers) {

	my $coderef = sub
	    { if ($theory->solvable_with ($prover,
					  $opt_solution_szs_status,
					  \%parameters)) {
		exit 0;
	    } else {
		exit 1;
	}
	  };
	my $harness = harness ($coderef);
	$harnesses{$prover} = $harness;
    }


    my $timer = timer ($opt_timeout);

    # Launch all the provers
    my @harnesses = values %harnesses;
    foreach my $harness (@harnesses) {
	$harness->start ();
    }

    $timer->start ();

    # Wait for a prover to terminate until the clock runs out
    until ((! $timer->check ()) || (any { ! $_->pumpable () } @harnesses)) {

	# Pump
	foreach my $harness (@harnesses) {
	    if ($harness->pumpable ()) {
		$harness->pump_nb ();
	    }
	}

	sleep 1;

    }

    # Finish the first nonpumpable harness.  Kill all the others
    foreach my $harness (@harnesses) {
	if ($harness->pumpable ()) {
	    $harness->kill_kill ();
	} else {
	    $harness->finish ();
	}
    }

    # carp 'Harnesses:', $LF, Dumper (%harnesses);

    foreach my $prover (keys %harnesses) {
	my $h = $harnesses{$prover};
	my @results = $h->full_results ();
	if (scalar @results == 0) {
	    # warn 'Zero results (or ', $prover, ' is still running).';
	} elsif (scalar @results == 1) {
	    my $result = $results[0];
	    if (defined $result) {
		if ($result == 0) {
		    return 1;
		}
	    }
	} else {
	    # warn 'Huh? Multiple results for ', $prover;
	}
    }

    return 0;

}

sub one_tool_countersolves {

    my $theory = shift;

    my %harnesses = ();
    my %parameters = ( 'timeout' => $opt_timeout );

    foreach my $prover (@opt_provers) {

	my $coderef = sub
	    { if ($theory->countersolvable_with ($prover,
						 $opt_solution_szs_status,
						 \%parameters)) {
		exit 0;
	    } else {
		exit 1;
	    }
	  };
	my $harness = harness ($coderef);
	$harnesses{$prover} = $harness;
    }


    my $timer = timer ($opt_timeout);

    # Launch all the provers
    my @harnesses = values %harnesses;
    foreach my $harness (@harnesses) {
	$harness->start ();
    }

    $timer->start ();

    # Wait for a prover to terminate until the clock runs out
    until ((! $timer->check ()) || (any { ! $_->pumpable () } @harnesses)) {

	# Pump
	foreach my $harness (@harnesses) {
	    if ($harness->pumpable ()) {
		$harness->pump_nb ();
	    }
	}

	sleep 1;

    }

    # Finish the first nonpumpable harness.  Kill all the others
    foreach my $harness (@harnesses) {
	if ($harness->pumpable ()) {
	    $harness->kill_kill ();
	} else {
	    $harness->finish ();
	}
    }

    # carp 'Harnesses:', $LF, Dumper (%harnesses);

    foreach my $prover (keys %harnesses) {
	my $h = $harnesses{$prover};
	my @results = $h->full_results ();
	if (scalar @results == 0) {
	    # warn 'Zero results (or ', $prover, ' is still running).';
	} elsif (scalar @results == 1) {
	    my $result = $results[0];
	    if (defined $result) {
		if ($result == 0) {
		    return 1;
		}
	    }
	} else {
	    # warn 'Huh? Multiple results for ', $prover;
	}
    }

    return 0;

}

sub one_prover_solves {
    my $theory = shift;
    return one_tool_solves ($theory);
}

sub one_prover_countersolves {
    my $theory = shift;
    my $parameters_ref = shift;
    return one_tool_countersolves ($theory,
				   \@opt_provers,
				   { 'timeout' => $opt_timeout });
}

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $theory_path = $arguments[0];
    my $theory = Theory->new (path => $theory_path);

    my @axioms = $theory->get_axioms (1);

    my $theory_has_conjecture = $theory->has_conjecture_formula ();
    my $conjecture = undef;
    my $conjecture_name = undef;
    if ($theory->has_conjecture_formula ()) {
	$conjecture = $theory->get_conjecture ();
	$conjecture_name = $conjecture->get_name ();
    }

    if (scalar @axioms == 0) {
	if (defined $conjecture) {
	    print colored ('Step 1', 'blue'), ': Derive the conjecture:', $SPACE;
	} else {
	    print colored ('Step 1', 'blue'), ': Solve the problem:', $SPACE;
	}
    } elsif (scalar @axioms == 1) {
	if (defined $conjecture) {
	    print colored ('Step 1', 'blue'), ': Derive the conjecture the sole available premises:', $SPACE;
	} else {
	    print colored ('Step 1', 'blue'), ': Solve the problem from the sole available premise:', $SPACE;
	}
    } else {
	if (defined $conjecture) {
	    print colored ('Step 1', 'blue'), ': Derive the conjecture from all ', scalar @axioms, ' available premises:', $SPACE;
	} else {
	    print colored ('Step 1', 'blue'), ': Solve the problem from all ', scalar @axioms, ' available premises:', $SPACE;
	}
    }

    my %initial_proof_szs_status = ();
    my %used_by_prover = ();
    my %unused_by_prover = ();

    my $num_initial_proofs_found = 0;

    foreach my $prover (@opt_provers) {

	my $initial_proof_result = undef;
	my $initial_proof_szs_status = undef;

	if ($opt_skip_initial_proof) {
	    $initial_proof_szs_status{$prover} = $SZS_NOT_TRIED;
	} else {

	    my $initial_proof_result = minimize ($theory, $prover);
	    $initial_proof_szs_status = $initial_proof_result->get_szs_status ();
	    $initial_proof_szs_status{$prover} = $initial_proof_szs_status;

	    if (is_szs_success ($initial_proof_szs_status)) {
		if (szs_implies ($initial_proof_szs_status,
				 $opt_solution_szs_status)) {

		    $num_initial_proofs_found++;

		    my $derivation = eval { $initial_proof_result->output_as_derivation (); };
		    my $derivation_message = $@;

		    my @used_premises = undef;
		    my @unused_premises = ();

		    if (defined $derivation) {
			@used_premises = $derivation->get_used_premises ();
			foreach my $axiom (@axioms) {
			    my $axiom_name = $axiom->get_name ();
			    if (none { $_->get_name eq $axiom_name } @used_premises) {
				push (@unused_premises, $axiom);
			    }
			}
		    } else {
			say warning_message ('Although the proof attempt with', $SPACE, $prover, $SPACE, 'succeeded (the SZS status was', $SPACE, $initial_proof_szs_status, '),');
			say 'we failed to extract a derivation, so we were unable to determine used premises.';
			say 'We will now proceed as though', $SPACE, $prover, $SPACE, 'used all available premises.';

			if ($opt_debug) {
			    if ($derivation_message eq $EMPTY_STRING) {
				say 'We somehow have nothing to report.';
			    } else {
				say 'Here is the message we got when trying to interpret the', $SPACE, $prover, $SPACE, 'output as a derivation:',;
				say $derivation_message;
			    }
			}

			@used_premises = @axioms;
		    }

		    $used_by_prover{$prover} = \@used_premises;
		    $unused_by_prover{$prover} = \@unused_premises;

		} else {
		    say error_message ('Prover', $SPACE, $prover, $SPACE, 'returned the SZS status', "\N{LF}", "\N{LF}", $TWO_SPACES, $initial_proof_szs_status, "\N{LF}", "\N{LF}", 'which does not imply the intended SZS status', "\N{LF}", "\N{LF}", $TWO_SPACES, $opt_solution_szs_status, "\N{LF}", "\N{LF}", 'We cannot proceed.  If you wish to skip the initial proof check, use the --skip-initial-proof option.');
		    exit 1;
		}
	    } else {
		$initial_proof_szs_status{$prover} = $SZS_UNKNOWN;
	    }
	}
    }

    if ($num_initial_proofs_found > 0) {
	say colored ('OK', $GOOD_COLOR);
    } elsif ($opt_skip_initial_proof) {
	say colored ('Skipped', $UNKNOWN_COLOR);
    } else {
	say colored ('Not OK', $BAD_COLOR);
	my @supported_provers = supported_provers ();
	say error_message ('No prover succeeded; we cannot proceed.');
	say 'We tried to solve the problem using the following prover(s):', "\N{LF}";
	say asterisk_list (@opt_provers);
	say 'If you wish to skip the initial proof check, use the --skip-initial-proof option.';
	say 'If you want to use other provers, use the --proof-finder option.';

	say 'Here are the supported provers:', "\N{LF}";
	say asterisk_list (@supported_provers);
	say 'One can supply multiple provers; just specify the ones you want to use using multiple --proof-finder options.';
	exit 1;
    }

    foreach my $prover (@opt_provers) {

	say 'PREMISES (', $prover, ')', $SPACE, '(', colored ('used', $USED_PREMISE_COLOR), $SPACE, '/', $SPACE, colored ('unused', $UNUSED_PREMISE_COLOR), ')';

	my $szs_status = $initial_proof_szs_status{$prover};

	if (is_szs_success ($szs_status)) {
	    my @used_premises = @{$used_by_prover{$prover}};
	    my @unused_premises = @{$unused_by_prover{$prover}};

	    if (scalar @used_premises > 0) {
		print_formula_names_with_color (\@used_premises,
						$USED_PREMISE_COLOR,
						{
						    'sorted' => 1 });
	    }
	    if (scalar @unused_premises > 0) {
		print_formula_names_with_color (\@unused_premises,
						$UNUSED_PREMISE_COLOR,
						{
						    'sorted' => 1 });
	    }
	} else {
	    say '(Nothing to report; the SZS status for', $SPACE, $prover, $SPACE, 'was', $SPACE, $szs_status, '.)';
	}
    }

    # Find the minimal sets of used premises
    my @minimal_used_premise_sets = ();
    foreach my $prover (@opt_provers) {
	my $szs_status = $initial_proof_szs_status{$prover};
	if (is_szs_success ($szs_status)) {
	    my @used_premises = @{$used_by_prover{$prover}};
	    my @used_premises_names = map { $_->get_name () } @used_premises;
	    my @used_premises_names_sorted = sort @used_premises_names;
	    if (all { my $other_prover = $_;
		      my $other_prover_szs_status
			  = $initial_proof_szs_status{$other_prover};
		      $other_prover eq $prover
			  || ! (is_szs_success ($other_prover_szs_status))
			      || eval { my @other_used_premises
					    = @{$used_by_prover{$other_prover}};
					my @other_used_premises_names
					    = map { $_->get_name () } @other_used_premises;
					my @other_used_premises_names_sorted
					    = sort @other_used_premises_names;
					subtuple (\@used_premises_names_sorted,
						  \@other_used_premises_names_sorted)
					    || ! subtuple (\@other_used_premises_names_sorted,
							   \@used_premises_names_sorted) } }
		    @opt_provers) {
		push (@minimal_used_premise_sets, \@used_premises_names_sorted);
	    }

	}
    }

    # carp 'Minimal used premise sets:', "\N{LF}", Dumper (@minimal_used_premise_sets);

    my %appearing_in_a_minimal_set = ();

    foreach my $tuple_ref (@minimal_used_premise_sets) {
	my @tuple = @{$tuple_ref};
	foreach my $premise (@tuple) {
	    $appearing_in_a_minimal_set{$premise} = 0;
	}
    }

    my @used_premises = keys %appearing_in_a_minimal_set;

    my @unused_premises = ();

    if (! $opt_skip_initial_proof) {
	foreach my $premise (@axioms) {
	    my $premise_name = $premise->get_name ();
	    if (defined $appearing_in_a_minimal_set{$premise_name}) {
		# ignore
	    } else {
		push (@unused_premises, $premise_name);
	    }
	}
    }

    $theory = $theory->remove_formulas_by_name (@unused_premises);

    if ($opt_skip_initial_proof) {
	@axioms = map { $_->get_name () } @axioms;
	@used_premises = @axioms;
    } else {
	@axioms = @used_premises;
    }

    @used_premises = sort @used_premises;
    @unused_premises = sort @unused_premises;
    @axioms = sort @axioms;

    # carp 'Axioms now:', Dumper (@axioms);

    my %needed = ();
    my %unneeded = ();
    my %unknown = ();

    if ($num_initial_proofs_found == 1) {
	say colored ('Step 2', 'blue'), ': From the', $SPACE, scalar @axioms, $SPACE, colored ('used', $USED_PREMISE_COLOR), $SPACE, 'premise(s) of the initial proof, determine the', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'ones.';
    } else {
	say colored ('Step 2', 'blue'), ': From the', $SPACE, scalar @axioms, $SPACE, colored ('used', $USED_PREMISE_COLOR), $SPACE, 'premise(s) of the initial proofs, determine the', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'ones.';
	say '(We count as', $SPACE, colored ('used', $USED_PREMISE_COLOR), $SPACE, 'those premises that appear in a minimal element';
	say 'of the partial order of sets of used premises, ordered by inclusion.)'
    }

    if (scalar @axioms == 0) {
	say 'We are immediately done, because the set of', $SPACE, colored ('used', $USED_PREMISE_COLOR), $SPACE, 'premises is empty.';
    } else {

	print 'PREMISES (', colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unknown', $UNKNOWN_COLOR), ')', "\N{LF}";

	foreach my $axiom (@axioms) {
	    if (defined $conjecture_name && $axiom eq $conjecture_name) {
		# Don't remove the conjecture (if there is one)
		next;
	    } else {
		my $trimmed_theory = $theory->remove_formula_by_name ($axiom);
		if (one_prover_countersolves ($trimmed_theory)) {
		    say colored ($axiom, $NEEDED_PREMISE_COLOR);
		    $needed{$axiom} = 0;
		} elsif (one_prover_solves ($trimmed_theory)) {
		    say colored ($axiom, $UNNEEDED_PREMISE_COLOR);
		    $unneeded{$axiom} = 0;
		} else {
		    say colored ($axiom, $UNKNOWN_COLOR);
		    $unknown{$axiom} = 0;
		}
	    }
	}

    }

    # Dump everything that is not known to be needed
    my $small_theory = $theory->copy ();
    foreach my $axiom (@axioms) {
	if (defined $needed{$axiom}) {
	    if ($opt_debug) {
		say {*STDERR} $axiom, $SPACE, 'is needed, so we are keeping it.';
	    }
	} elsif (defined $conjecture_name && $axiom eq $conjecture_name) {
	    # Don't remove the conjecture (if there is one)
	} else {
	    $small_theory = $small_theory->remove_formula_by_name ($axiom);
	    if ($opt_debug) {
		say {*STDERR} $axiom, $SPACE, 'is not needed, so it has been removed.';
	    }
	}
    }

    if (defined $conjecture) {
	print colored ('Step 3', 'blue'), ': Derive the conjecture from only the', $SPACE, scalar keys %needed, $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premise(s):', $SPACE;
    } else {
	print colored ('Step 3', 'blue'), ': Solve the problem from only the', $SPACE, scalar keys %needed, $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premise(s):', $SPACE;
    }

    if ($opt_debug) {
	warn 'The minimal theory is', "\N{LF}", Dumper ($small_theory);
    }

    if (one_prover_solves ($small_theory)) {

	say colored ('OK', $GOOD_COLOR);

	if (defined $conjecture) {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone suffice to prove the conjecture; we are done.';
	} else {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone suffice to solve the problem; we are done.';
	}

	say 'All', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $SPACE, 'and', $SPACE, colored ('unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, 'premises can be safely removed.';

    } else {

	say colored ('Not OK', $BAD_COLOR);

	if (defined $conjecture) {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone do not suffice to prove the conjecture.';
	} else {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone do not suffice to solve the problem.';
	}

	if (defined $conjecture) {
	    say colored ('Step 4', 'blue'), ': Find combinations of', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $SPACE, 'and', $SPACE, colored ('unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, 'premises that, together with the', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premises, suffice to derive the conjecture.';
	} else {
	    say colored ('Step 4', 'blue'), ': Find combinations of', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $SPACE, 'and', $SPACE, colored ('unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, 'premises that, together with the', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premises, suffice to solve the problem.';
	}

	my @unknown_formulas = keys %unknown;
	my @unneeded_formulas = keys %unneeded;

	my %candidates = ();
	foreach my $formula (@unknown_formulas, @unneeded_formulas) {
	    $candidates{$formula} = 0;
	}
	my @candidate_formulas = keys %candidates;

	my @solved_so_far_positively = ();
	my @solved_so_far_negatively = ();

	my $num_combinations = 2 ** scalar @candidate_formulas;
	my $estimate_seconds = $opt_timeout * $num_combinations;
	my $estimate_minutes = ceil ($estimate_seconds / 60);
	my $estimate_minutes_at_least_one
	    = $estimate_minutes < 1 ? 1 : $estimate_minutes;

	if ($num_combinations == 1) {
	    say 'There is only 1 combination to check.';
	    say 'Be patient; in the worst case, evaluating it will take', $SPACE, $estimate_minutes_at_least_one, $SPACE, 'minute(s).';
	} else {
	    say 'There are', $SPACE, $num_combinations, $SPACE, 'combinations to check.';
	    say 'Be patient; in the worst case, evaluating all of them will take', $SPACE, $estimate_minutes_at_least_one, $SPACE, 'minute(s).';
	}

	say '(It will probably take much less.)';

	my $progress = Term::ProgressBar->new ({ count => $num_combinations });
	my $num_tuples_handled = 0;
	my $num_candidates_unknown = 0;
	my $num_already_known_good = 0;
	$progress->update (0);

	foreach my $k (0 .. scalar @candidate_formulas) {

	    if ($opt_debug) {
		carp 'Iterating combinations of size', $SPACE, $k;
	    }

	    my $candidate_iterator = combinations (\@candidate_formulas, $k);

	    while (defined (my $tuple_ref = $candidate_iterator->next)) {

		$num_tuples_handled++;
		$progress->update ($num_tuples_handled);

		my @tuple = @{$tuple_ref};

		if (scalar @tuple == 0) {
		    $progress->update ($num_tuples_handled);
		    next;
		}

		my $already_known_good
		    = any { subtuple ($_, \@tuple); } @solved_so_far_positively;

		if ($already_known_good) {
		    $num_already_known_good++;
		    next;
		}

		my @formulas = map { $theory->formula_with_name ($_) } @tuple;
		my $bigger_theory = $small_theory->postulate (\@formulas);

		if (one_prover_countersolves ($bigger_theory)) {
		    push (@solved_so_far_negatively, \@tuple);
		} elsif (one_prover_solves ($bigger_theory)) {
		    push (@solved_so_far_positively, \@tuple);
		} else {
		    # Don't know what to do
		    $num_candidates_unknown++;
		}

	    }

	}

	$progress->update ($num_combinations);

	if (scalar @solved_so_far_positively == 0) {
	    say 'Unfortunately, we found no proper subtheories of the original theory';
	    say 'that contain all', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'premises';
	    say 'and which derive the conjecture.';
	} else {

	    say 'We found', $SPACE, scalar @solved_so_far_positively, $SPACE, 'solution(s).';

	    my @solved_sorted = sort { subtuple ($a, $b) } @solved_so_far_positively;

	    my %sufficient_additional_axioms;
	    foreach my $tuple_ref (@solved_so_far_positively) {
		my @tuple = @{$tuple_ref};
		foreach my $formula_name (@tuple) {
		    $sufficient_additional_axioms{$formula_name} = 0;
		}
	    }

	    my @sufficient_additional_axioms = keys %sufficient_additional_axioms;
	    my @sorted_sufficient_additional_axioms
		= sort @sufficient_additional_axioms;


	    print_solvable_supertheories (\@solved_sorted,
					  \@sorted_sufficient_additional_axioms);

	    say 'Along the way, we found', $SPACE, $num_candidates_unknown, $SPACE, 'combinations of premises';
	    say 'for which we could not determine whether they constitute a solution.';
	    say 'There were', $SPACE, $num_already_known_good, $SPACE, 'combinations that properly extend known good combinations,';
	    say 'so they were not printed.';

	    if ($opt_confirm) {
		say 'Confirming minimality of the', $SPACE, scalar @solved_sorted, $SPACE, 'solutions.';
		print 'PREMISES (', colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unknown', $UNKNOWN_COLOR), ')', "\N{LF}";
		foreach my $i (1 .. scalar @solved_sorted) {
		    my $solution_ref = $solved_sorted[$i - 1];
		    my @solution = @{$solution_ref};
		    my @formulas = map { $theory->formula_with_name ($_) } @solution;
		    my $bigger_theory = $small_theory->postulate (\@formulas);
		    say 'Solution', $SPACE, $i;
		    foreach my $premise (@solution) {
			my $bad_theory
			    = $bigger_theory->remove_formula_by_name ($premise);
			if (one_prover_countersolves ($bad_theory)) {
			    say colored ($premise, $NEEDED_PREMISE_COLOR)
			} elsif (one_prover_solves ($bad_theory)) {
			    say colored ($premise, $UNNEEDED_PREMISE_COLOR);
			} else {
			    say colored ($premise, $UNKNOWN_COLOR);
			}
		    }
		}
	    }

	}

    }

    return 1;

}

sub print_solvable_supertheories {

    my $solutions_ref = shift;
    my $axioms_ref = shift;

    my @solutions = @{$solutions_ref};
    my @axioms = @{$axioms_ref};

    my $num_solutions = scalar @solutions;

    if ($num_solutions == 0) {
	return;
    }

    my @axiom_lengths = map { length $_ } @axioms;
    my $length_of_longest_axiom = max @axiom_lengths;

    print fill_up_to_column ('PREMISE', $length_of_longest_axiom), $SPACE;
    foreach my $i (1 .. $num_solutions) {
	print '|', $SPACE, 'Solution', $SPACE, $i, $SPACE;
    }

    say '|';

    foreach my $axiom (@axioms) {
	print fill_up_to_column ($axiom, $length_of_longest_axiom), $SPACE;
	foreach my $solution_ref (@solutions) {
	    my @solution = @{$solution_ref};
	    if (any { $_ eq $axiom } @solution) {
		print '|', $SPACE x 5, 'x', $SPACE x 6;
	    } else {
		print '|', $SPACE x 12;
	    }
	}

	print '|', "\N{LF}";

    }

    return;
}

    1;
__END__
