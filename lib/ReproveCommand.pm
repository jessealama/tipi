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
Readonly my $DESCRIPTION => 'Prove a conjecture again, perhaps using different premises.';
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
my $opt_syntactically = 0;
my $opt_semantically = 0;
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
	'semantically' => \$opt_semantically,
	'syntactically' => \$opt_syntactically,
	'solution-szs-status=s' => \$opt_solution_szs_status,
	'model-finder-timeout=i' => \$opt_model_finder_timeout,
	'proof-finder-timeout=i' => \$opt_proof_finder_timeout,
	'only-used' => \$opt_show_only_final_used_premises,
	'only-unused' => \$opt_show_only_final_unused_premises,
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

    if ($opt_syntactically && $opt_semantically) {
	pod2usage (-msg => error_message ('Please choose which reprove procedure you would like to use (the two options are --syntactically and --semantically).'),
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

    if ($opt_syntactically) {
	return reprove_syntactically ($theory);
    } elsif ($opt_semantically) {
	return reprove_semantically ($theory);
    } else {
	say STDERR error_message ('We need to know which reprove method should be use; somehow, neither of the two methods was selected.');
	exit 1;
    }

}

sub reprove_syntactically {

    my $theory = shift;

    my $derivation = prove_if_possible ($theory,
					{ 'timeout' => $opt_proof_finder_timeout });

    if ($opt_debug) {
	print {*STDERR} 'The derivation was just obtained:', "\N{LF}", Dumper ($derivation);
    }

    if (! $opt_show_only_final_used_premises
	    && ! $opt_show_only_final_unused_premises) {
	print 'PREMISES (', colored ('used', $USED_PREMISE_COLOR), ' / ', colored ('unused', $UNUSED_PREMISE_COLOR), ')', "\N{LF}";
    }

    my @unused_premises = $derivation->get_unused_premises ();

    while (scalar @unused_premises > 0) {

	if (! $opt_show_only_final_used_premises) {
	    print_formula_names_with_color (\@unused_premises, $UNUSED_PREMISE_COLOR);
	}

	$theory = $derivation->theory_from_used_premises ();
	$derivation = prove_if_possible ($theory);
	@unused_premises = $derivation->get_unused_premises ();

    }

    my @used_premises = $derivation->get_used_premises ();

    if ($opt_show_only_final_used_premises) {
	print_formula_names_with_color (\@used_premises);
    } elsif ($opt_show_only_final_unused_premises) {
	# nothing to do
    } else {
	print_formula_names_with_color (\@used_premises, $USED_PREMISE_COLOR);
    }

    return 1;

}

sub reprove_semantically {
    my $theory = shift;

    my $theory_path = $theory->get_path ();

    my @axioms = $theory->get_axioms (1);

    my $theory_has_conjecture = $theory->has_conjecture_formula ();
    my $conjecture = undef;
    if ($theory->has_conjecture_formula ()) {
	$conjecture = $theory->get_conjecture ();
    }

    if ($theory->has_conjecture_formula ()) {
	$theory = $theory->promote_conjecture_to_false_axiom ();
    }

    my %needed = ();
    my %unneeded = ();
    my %unknown = ();

    print 'PREMISES (', colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unknown', $UNKNOWN_COLOR), ')', "\N{LF}";

    my @sorted_axioms = sort { $a->get_name() cmp $b->get_name () } @axioms;

    foreach my $axiom (@sorted_axioms) {
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
	print 'Now try deriving the conjecture from only the', $SPACE, scalar keys %needed, $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premise(s):';
    } else {
	print 'Now try solving the problem from only the', $SPACE, scalar keys %needed, $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premise(s):';
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

    my $new_result = TPTP::prove ($small_theory,
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
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone suffice to prove the conjecture.';
	} else {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone suffice to solve the problem.';
	}
    } else {
	if (defined $conjecture) {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone do not suffice to prove the conjecture.';
	} else {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone do not suffice to solve the problem.';
	}
	say 'Consider the minimize command to search for combinations of', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $SPACE, 'and', $SPACE, colored ('unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, 'formulas';
	say 'that suffice to solve the problem.';

    }

    return 1;

}

1;
__END__
