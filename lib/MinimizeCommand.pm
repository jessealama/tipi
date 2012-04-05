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
use List::MoreUtils qw(any);
use Term::ProgressBar;
use Regexp::DefaultFlags;
use POSIX qw(ceil);
use Algorithm::Combinatorics qw(combinations);

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    );
use Utils qw(error_message
	     all_sublists
	     all_nonempty_sublists
	     remove_duplicate_lists
	     subtuple
	     tuple_less_than_wrt_ordering
	     ensure_readable_file);

Readonly my $EMPTY_STRING => q{};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $COMMA => q{,};
Readonly my $COLON => q{:};
Readonly my $SLASH => q{/};
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'yellow';
Readonly my $DESCRIPTION => 'Find minimal subtheories.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';
Readonly my $NEEDED_PREMISE_COLOR => 'red';
Readonly my $UNNEEDED_PREMISE_COLOR => 'cyan';

my $opt_show_output = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_solution_szs_status = 'Theorem';
my $opt_proof_finder = 'eprover';
my $opt_model_finder_timeout = 5;
my $opt_proof_finder_timeout = 30;
my $opt_skip_initial_proof = 0;
my $opt_show_only_final_used_premises = 0;
my $opt_show_only_final_unused_premises = 0;

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
	'proof-finder=s' => \$opt_proof_finder,
	'solution-szs-status=s' => \$opt_solution_szs_status,
	'model-finder-timeout=i' => \$opt_model_finder_timeout,
	'proof-finder-timeout=i' => \$opt_proof_finder_timeout,
	'skip-initial-proof' => \$opt_skip_initial_proof,
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

    if ($opt_model_finder_timeout < 0) {
	pod2usage (-msg => error_message ('Invalid value ', $opt_model_finder_timeout, ' for the model-finder timeout option.'),
		   -exitval => 2);
    }

    if ($opt_proof_finder_timeout < 0) {
	pod2usage (-msg => error_message ('Invalid value ', $opt_proof_finder_timeout, ' for the proof-finder timeout option.'),
		   -exitval => 2);
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
	say STDERR error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	exit 1;
    }

    my $theory = Theory->new (path => $theory_path);

    if ($theory->is_first_order ()) {
	return $self->$orig (@arguments);
    } else {
	say {*STDERR} error_message ('The theory at', $SPACE, $theory_path, $SPACE, 'seems to be a non-first-order theory.');
    }

};

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $theory_path = $arguments[0];
    my $theory = Theory->new (path => $theory_path);

    my @axioms = $theory->get_axioms (1);

    my $theory_has_conjecture = $theory->has_conjecture_formula ();
    my $conjecture = undef;
    if ($theory->has_conjecture_formula ()) {
	$conjecture = $theory->get_conjecture ();
    }

    if (scalar @axioms == 0) {
	if (defined $conjecture) {
	    print colored ('Step 1', 'blue'), ': Derive the conjecture:';
	} else {
	    print colored ('Step 1', 'blue'), ': Solve the problem:';
	}
    } elsif (scalar @axioms == 1) {
	if (defined $conjecture) {
	    print colored ('Step 1', 'blue'), ': Derive the conjecture the sole available premises:';
	} else {
	    print colored ('Step 1', 'blue'), ': Solve the problem from the sole available premise:';
	}
    } else {
	if (defined $conjecture) {
	    print colored ('Step 1', 'blue'), ': Derive the conjecture from all ', scalar @axioms, ' available premises:';
	} else {
	    print colored ('Step 1', 'blue'), ': Solve the problem from all ', scalar @axioms, ' available premises:';
	}
    }

    my $initial_proof_result = undef;
    my $initial_proof_szs_status = undef;

    if ($opt_skip_initial_proof) {
	say $SPACE, colored ('OK', 'green'), $SPACE, '(No proof was attempted because you requested that we skip it.)';
	$initial_proof_szs_status = $opt_solution_szs_status;
    } else {

	$initial_proof_result = TPTP::prove ($theory, $opt_proof_finder);
	$initial_proof_szs_status
	    = $initial_proof_result->has_szs_status ? $initial_proof_result->get_szs_status ()
		: 'Unknown';

	if ($initial_proof_szs_status eq $opt_solution_szs_status) {
	    say $SPACE, colored ('OK', 'green');
	} else {
	    say $SPACE, colored ('Not OK', 'red'), ' (SZS status ', $initial_proof_szs_status, ')';
	    say 'Use --skip-initial-proof to skip the initial proof and treat all';
	    say 'the axioms of the theory as "used".';
	    return 1;
	}


    }

    if ($opt_skip_initial_proof) {
	say 'PREMISES', $SPACE, '(', colored ('used', $USED_PREMISE_COLOR), $SPACE, '/', $SPACE, colored ('unused', $UNUSED_PREMISE_COLOR), ')';
	print_formula_names_with_color (\@axioms, $USED_PREMISE_COLOR);
	say '(As requested, we skipped the initial proof.  This means that we will now regard';
	say 'all the theory\'s axioms as', $SPACE, colored ('used', $USED_PREMISE_COLOR), ', even if';
	say 'a derivation is available that uses only some of them.)';
    } else {
	my $derivation = $initial_proof_result->output_as_derivation ();
	my @used_premises = $derivation->get_used_premises ();
	my @unused_premises = $derivation->get_unused_premises ();

	say 'PREMISES', $SPACE, '(', colored ('used', $USED_PREMISE_COLOR), $SPACE, '/', $SPACE, colored ('unused', $UNUSED_PREMISE_COLOR), ')';

	if (scalar @used_premises > 0) {
	    print_formula_names_with_color (\@used_premises,
					    $USED_PREMISE_COLOR,
					    { 'sorted' => 1 });
	}
	if (scalar @unused_premises > 0) {
	    print_formula_names_with_color (\@unused_premises,
					    $UNUSED_PREMISE_COLOR,
					    { 'sorted' => 1 });
	}

	$theory = $theory->remove_formulas (@unused_premises);

	if ($opt_debug) {
	    say {*STDERR} 'After removing', "\N{LF}", join ("\N{LF}", @unused_premises), "\N{LF}", 'the theory now looks like this:', "\N{LF}", Dumper ($theory);
	}

	@axioms = @used_premises;
    }

    if ($theory->has_conjecture_formula ()) {
	$theory = $theory->promote_conjecture_to_false_axiom ();
    }

    my @axiom_names = map { $_->get_name () } @axioms;

    my %needed = ();
    my %unneeded = ();
    my %unknown = ();

    say colored ('Step 2', 'blue'), ': From the', $SPACE, scalar @axioms, $SPACE, colored ('used', $USED_PREMISE_COLOR), $SPACE, 'premise(s) of the initial proof, determine the', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'ones.';

    print 'PREMISES (', colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unknown', $UNKNOWN_COLOR), ')', "\N{LF}";

    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	my $trimmed_theory = $theory->remove_formula ($axiom);
	my $satisfiable = $trimmed_theory->is_satisfiable ({'timeout' => $opt_model_finder_timeout});

	if ($satisfiable == -1) {
	    say colored ($axiom_name, $UNKNOWN_COLOR);
	    $unknown{$axiom_name} = 0;
	} elsif ($satisfiable == 0) {
	    say colored ($axiom_name, $UNNEEDED_PREMISE_COLOR);
	    $unneeded{$axiom_name} = 0;
	} else {
	    say colored ($axiom_name, $NEEDED_PREMISE_COLOR);
	    $needed{$axiom_name} = 0;
	}

    }

    if (defined $conjecture) {
	print colored ('Step 3', 'blue'), ': Derive the conjecture from only the', $SPACE, scalar keys %needed, $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premise(s):';
    } else {
	print colored ('Step 3', 'blue'), ': Solve the problem from only the', $SPACE, scalar keys %needed, $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premise(s):';
    }

    # Dump everything that is not known to be needed
    my $small_theory = $theory->copy ();
    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	if (defined $needed{$axiom_name}) {
	    if ($opt_debug) {
		say {*STDERR} $axiom_name, $SPACE, 'is needed, so we are keeping it.';
	    }
	} else {
	    $small_theory = $small_theory->remove_formula ($axiom);
	    if ($opt_debug) {
		say {*STDERR} $axiom_name, $SPACE, 'is not needed, so it has been removed.';
	    }
	}
    }

    # Remove the old conjecture, which was promoted to a false axiom,
    # and put it back as the conjecture.
    if (defined $conjecture) {
	$small_theory = $small_theory->strip_conjecture ($conjecture);
	$small_theory = $small_theory->add_formula ($conjecture);
    }

    if ($opt_debug) {
	warn 'The minimal theory is', "\N{LF}", Dumper ($small_theory);
    }

    if (defined $conjecture && ! $small_theory->has_conjecture_formula ()) {
	my $small_theory_path = $small_theory->get_path ();
	croak 'The theory ', $small_theory_path, ' has no conjecture formula!';
    }

    my $new_result = TPTP::prove ($small_theory,
				  $opt_proof_finder,
				  { 'timeout' => $opt_proof_finder_timeout });
    my $new_result_szs_status
	= $new_result->has_szs_status () ? $new_result->get_szs_status () : 'Unknown';

    if ($new_result_szs_status eq $opt_solution_szs_status) {
	say $SPACE, colored ('OK', $GOOD_COLOR);
    } else {
	say $SPACE, colored ('Not OK', $BAD_COLOR), ' (SZS status ', $new_result_szs_status, ')';
    }

    if ($new_result_szs_status eq $opt_solution_szs_status) {
	if (defined $conjecture) {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone suffice to prove the conjecture; we are done.';
	} else {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone suffice to solve the problem; we are done.';
	}
    } else {
	if (defined $conjecture) {
	    say 'The needed formulas alone do not suffice to prove the conjecture.';
	} else {
	    say 'The needed formulas alone do not suffice to solve the problem.';
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
	my $estimate_seconds = $opt_proof_finder_timeout * $num_combinations;
	my $estimate_minutes = ceil ($estimate_seconds / 60);
	my $estimate_minutes_at_least_one
	    = $estimate_minutes < 1 ? 1 : $estimate_minutes;

	say 'There are', $SPACE, $num_combinations, $SPACE, 'combinations to check.';
	say 'Be patient; in the worst case, evaluating all of them will take', $SPACE, $estimate_minutes_at_least_one, $SPACE, 'minute(s).';

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

		my $bigger_theory_conjecture_false
		    = $bigger_theory->promote_conjecture_to_false_axiom ();
		my $conjecture_false_satisfiable
		    = $bigger_theory_conjecture_false->is_satisfiable ({ 'timeout' => $opt_model_finder_timeout });

		if ($conjecture_false_satisfiable == -1) {

		    # model finder did not give an answer; let's use a proof finder
		    my $bigger_result = TPTP::prove ($bigger_theory,
						     $opt_proof_finder,
						     { 'timeout' => $opt_proof_finder_timeout });
		    my $bigger_result_szs_status
			= $bigger_result->has_szs_status () ?
			    $bigger_result->get_szs_status ()
				: 'Unknown';
		    if ($bigger_result_szs_status eq $opt_solution_szs_status) {
			say 'Solution found:', "\N{LF}", join ("\N{LF}", @tuple), "\N{LF}";
			if ($opt_debug) {
			    say {*STDERR} 'The theory where this was proved is:', "\N{LF}", Dumper ($bigger_theory);
			}
			push (@solved_so_far_positively, \@tuple);
		    } else {
			$num_candidates_unknown++;
		    }

		} elsif ($conjecture_false_satisfiable == 0) {
		    push (@solved_so_far_positively, \@tuple);
		} else {
		    push (@solved_so_far_negatively, \@tuple);
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
