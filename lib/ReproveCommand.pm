package ReproveCommand;

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

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    );
use Utils qw(error_message
	     all_nonempty_sublists
	     subtuple
	     tuple_less_than_wrt_ordering
	     ensure_readable_file);

Readonly my $EMPTY_STRING => q{};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $COMMA => q{,};
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'yellow';
Readonly my $DESCRIPTION => 'Prove a conjecture again, perhaps using different premises.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';
Readonly my $NEEDED_PREMISE_COLOR => 'red';
Readonly my $UNNEEDED_PREMISE_COLOR => 'yellow';

my $opt_show_output = 0;
my $opt_show_premises = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_solution_szs_status = 'Theorem';
my $opt_method = 'syntactically';
my $opt_model_finder_timeout = 5;
my $opt_proof_finder_timeout = 30;
my $opt_force = 0;
my $opt_semantically_use_all_axioms = 0;

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
	'method=s' => \$opt_method,
	'solution-szs-status=s' => \$opt_solution_szs_status,
	'model-finder-timeout=i' => \$opt_model_finder_timeout,
	'proof-finder-timeout=i' => \$opt_proof_finder_timeout,
	'use-all-axioms' => \$opt_semantically_use_all_axioms,
	'force' => \$opt_force,
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
	pod2usage (-msg => error_message ('Unable to make sense of the prove arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
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

    if ($opt_method ne 'syntactically' && $opt_method ne 'semantically') {
	pod2usage (-msg => error_message ('The only acceptable values for the --method option are \'syntactically\' and \'semantically\'.'),
		   -exitval => 2);
    }

    if ($opt_method ne 'semantically' && $opt_semantically_use_all_axioms) {
	pod2usage (-msg => error_message ('The --use-all-axioms option is applicable only when reproving semantically.'),
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

    if (ensure_sensible_tptp_theory ($theory_path)) {
	return $self->$orig (@arguments);
    } else {
	say STDERR error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	exit 1;
    }

};

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $theory_path = $arguments[0];
    my $theory = Theory->new (path => $theory_path);

    if ($opt_method eq 'syntactically') {
	return reprove_syntactically ($theory);
    } elsif ($opt_method eq 'semantically') {
	return reprove_semantically ($theory);
    } else {
	say STDERR error_message ('Unknown reprove method \'', $opt_method, '\'.');
	exit 1;
    }

}

sub reprove_syntactically {

    my $theory = shift;

    my $derivation = prove_if_possible ($theory);

    if ($opt_debug) {
	print {*STDERR} 'The derivation was just obtained:', "\N{LF}", Dumper ($derivation);
    }

    print 'PREMISES (', colored ('used', $USED_PREMISE_COLOR), ' / ', colored ('unused', $UNUSED_PREMISE_COLOR), ')', "\N{LF}";

    my @unused_premises = $derivation->get_unused_premises ();

    while (scalar @unused_premises > 0) {

	print_formula_names_with_color (\@unused_premises, $UNUSED_PREMISE_COLOR);

	$theory = $derivation->theory_from_used_premises ();
	$derivation = prove_if_possible ($theory);
	@unused_premises = $derivation->get_unused_premises ();

    }

    my @used_premises = $derivation->get_used_premises ();
    print_formula_names_with_color (\@used_premises, $USED_PREMISE_COLOR);

    return 1;

}

sub reprove_semantically {
    my $theory = shift;

    my $theory_path = $theory->get_path ();

    my @axioms = $theory->get_axioms (1);

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

    my $initial_proof_result = TPTP::prove ($theory);
    my $initial_proof_szs_status
 	= $initial_proof_result->has_szs_status ? $initial_proof_result->get_szs_status
	    : 'Unknown';

    if ($initial_proof_szs_status eq $opt_solution_szs_status) {
	say $SPACE, colored ('OK', 'green');
    } else {
	say $SPACE, colored ('Not OK', 'red'), ' (SZS status ', $initial_proof_szs_status, ')';
    }

    if ($initial_proof_szs_status ne $opt_solution_szs_status && $opt_force) {
	if (scalar @axioms == 0) {
	    say 'Even though we were requested to proceed, it does not make sense';
	    say 'to do so because there are no axioms in the background theory.';
	    return 1;
	} else {
	    say 'Continuing anyway, as requested; the results that follow may not be meaningful.';
	    if (scalar @axioms == 1) {
		my $axiom = $axioms[0];
		my $axiom_name = $axiom->get_name ();
		say 'Since we did not find a derivation, we will now inspect ', $axiom_name, ', treating each as potentially needed.';
	    } else {
		say 'Since we did not find a derivation, we will now inspect all ', scalar @axioms, ' axioms, treating each as potentially needed.';
	    }

	}
    }

    if ($initial_proof_szs_status ne $opt_solution_szs_status && ! $opt_force) {
	return 1;
    }

    if ($opt_semantically_use_all_axioms) {
	say 'PREMISES', $SPACE, '(', colored ('used', $USED_PREMISE_COLOR), $SPACE, '/', $SPACE, colored ('unused', $UNUSED_PREMISE_COLOR), ')';
	print_formula_names_with_color (\@axioms, $USED_PREMISE_COLOR);
	say '(As requested, we are regarding all the theory\'s axioms as used,';
	say 'even if only some of them were actually used in a successful derivation.)';
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

	@axioms = @used_premises;
    }

    if ($theory->has_conjecture_formula ()) {
	$theory = $theory->promote_conjecture_to_false_axiom ();
    }

    my %needed = ();
    my %unneeded = ();
    my %unknown = ();

    say colored ('Step 2', 'blue'), ': From the', $SPACE, scalar @axioms, $SPACE, colored ('used', $USED_PREMISE_COLOR), $SPACE, 'premises, determine the', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'premises.';

    print 'PREMISES (', colored ('needed', $NEEDED_PREMISE_COLOR), ' / ', colored ('unknown', $UNKNOWN_COLOR), ')', "\N{LF}";

    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	my $trimmed_theory = $theory->remove_formula ($axiom);
	my $tptp_result = eval
	    { TPTP::find_model ($trimmed_theory,
				{ 'timeout' => $opt_model_finder_timeout }) };
	my $tptp_find_model_message = $@;

	my $szs_status
	    = (defined $tptp_result && $tptp_result->has_szs_status ()) ? $tptp_result->get_szs_status () : 'Unknown';

	if (defined $tptp_result) {
	    if ($tptp_result->timed_out ()) {
		say colored ($axiom_name, $UNKNOWN_COLOR), $SPACE, '(SZS status', $SPACE, $szs_status, ')';
		$unknown{$axiom_name} = 0;
	    } elsif ($szs_status eq 'Satisfiable') {
		say colored ($axiom_name, $NEEDED_PREMISE_COLOR), $SPACE, '(SZS status', $SPACE, $szs_status, ')';
		$needed{$axiom_name} = 0;
	    } else {
		say colored ($axiom_name, $UNKNOWN_COLOR), $SPACE, '(SZS status', $SPACE, $szs_status, ')';
		$unknown{$axiom_name} = 0;
	    }
	} else {
	    say {*STDERR} warning_message ('Something went wrong when testing whether ', $axiom_name, ' can be removed:', "\N{LF}", $tptp_find_model_message);
	}

    }

    if ($theory->has_conjecture_formula ()) {
	print colored ('Step 3', 'blue'), ': Derive the conjecture from only ', colored ('needed', $NEEDED_PREMISE_COLOR), ' premises:';
    } else {
	print colored ('Step 3', 'blue'), ': Solve the problem from only ', colored ('needed', $NEEDED_PREMISE_COLOR), ' premises:';
    }

    # Dump everything that is not known to be needed
    my $small_theory = $theory;
    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	if (! defined $needed{$axiom_name}) {
	    $small_theory = $small_theory->remove_formula ($axiom);
	}
    }

    # Remove the old conjecture, which was promoted to a false axiom,
    # and put it back as the conjecture.
    if ($theory->has_conjecture_formula ()) {
	$small_theory = $small_theory->remove_formula ($conjecture);
	$small_theory = $small_theory->add_formula ($conjecture);
    }

    if ($opt_debug) {
	warn 'The minimal theory is', "\N{LF}", Dumper ($small_theory);
    }

    if ($theory->has_conjecture_formula () &&
	    ! $small_theory->has_conjecture_formula ()) {
	my $small_theory_path = $small_theory->get_path ();
	croak 'The theory ', $small_theory_path, ' has no conjecture formula!';
    }

    my $new_result = TPTP::prove ($small_theory,
				  { 'timeout' => $opt_proof_finder_timeout });
    my $new_result_szs_status
	= $new_result->has_szs_status () ? $new_result->get_szs_status () : 'Unknown';

    if ($new_result_szs_status eq $opt_solution_szs_status) {
	say $SPACE, colored ('OK', 'green');
    } else {
	say $SPACE, colored ('Not OK', 'red'), ' (SZS status ', $new_result_szs_status, ')';
    }

    if ($new_result_szs_status eq $opt_solution_szs_status) {
	if ($theory->has_conjecture_formula ()) {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone suffice to prove the conjecture; we are done.';
	} else {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone suffice to solve the problem; we are done.';
	}
    } else {
	if ($theory->has_conjecture_formula ()) {
	    say 'The needed formulas alone do not suffice to prove the conjecture.';
	} else {
	    say 'The needed formulas alone do not suffice to solve the problem.';
	}

	if ($theory->has_conjecture_formula ()) {
	    say colored ('Step 4', 'blue'), ': Find combinations of', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $SPACE, 'premises that, together with the', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premises, suffice to derive the conjecture.';
	} else {
	    say colored ('Step 4', 'blue'), ': Find combinations of', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $SPACE, 'premises that, together with the', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premises, suffice to solve the problem.';
	}

	my @unknown_formula_names = keys %unknown;
	my @unknown_formula_names_sorted = sort @unknown_formula_names;
	my @solved_so_far = ();

	my @tuples = all_nonempty_sublists (\@unknown_formula_names_sorted);
	my $num_combinations = scalar @tuples;

	say 'There are', $SPACE, $num_combinations, $SPACE, 'combinations to check.';
	say 'Be patient; in the worst case, evaluating these will take', $SPACE, $opt_proof_finder_timeout * $num_combinations, $SPACE, 'seconds.';

	my $progress = Term::ProgressBar->new ({ count => $num_combinations });
	my $num_tuples_handled = 0;
	my $num_candidates_unknown = 0;
	$progress->update ($num_tuples_handled);

	my @tuples_sorted
	    = sort { tuple_less_than_wrt_ordering ($a, $b, \@unknown_formula_names_sorted) } @tuples;

	foreach my $tuple_ref (@tuples_sorted) {

	    $num_tuples_handled++;

	    my @tuple = @{$tuple_ref};

	    if (scalar @tuple == 0) {
		$progress->update ($num_tuples_handled);
		next;
	    }

	    my $already_known = any { my @known_tuple = @{$_};
				      subtuple (\@known_tuple, \@tuple); } @solved_so_far;

	    if ($already_known) {
		$progress->update ($num_tuples_handled);
		next;
	    }

	    my @formulas = map { $theory->formula_with_name ($_) } @tuple;
	    my $bigger_theory = $small_theory->postulate (\@formulas);
	    my $bigger_result = TPTP::prove ($bigger_theory,
					     { 'timeout' => $opt_proof_finder_timeout });
	    my $bigger_result_szs_status
		= $bigger_result->has_szs_status () ?
		    $bigger_result->get_szs_status ()
			: 'Unknown';
	    if ($bigger_result_szs_status eq $opt_solution_szs_status) {
		push (@solved_so_far, \@tuple);
	    } else {
		$num_candidates_unknown++;
	    }

	    $progress->update ($num_tuples_handled);

	}

	$progress->update ($num_combinations);

	if (scalar @solved_so_far == 0) {
	    say 'Unfortunately, no solutions were found.';
	} else {

	    say 'We found', $SPACE, scalar @solved_so_far, $SPACE, 'solution(s).';
	    say 'There were', $SPACE, $num_candidates_unknown, $SPACE, 'non-solution combinations.  For these we either timed out evaluating them,';

	    if ($theory->has_conjecture_formula ()) {
		say 'or the candidate was shown to be too weak to derive the conjecture.';
	    } else {
		say 'or the candidate was shown to be too weak to solve the problem.';
	    }

	    my @solved_sorted = sort { subtuple ($a, $b) } @solved_so_far;

	    my %sufficient_additional_axioms;
	    foreach my $tuple_ref (@solved_so_far) {
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

    print '|', "\N{LF}";

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
