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

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available ensure_valid_tptp_file prove_if_possible ensure_sensible_tptp_theory);
use Utils qw(error_message);

Readonly my $EMPTY_STRING => q{};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'bright_black';
Readonly my $DESCRIPTION => 'Prove a conjecture again, perhaps using different premises.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';
Readonly my $NEEDED_PREMISE_COLOR => 'red';
Readonly my $UNNEEDED_PREMISE_COLOR => 'bright_black';

my $opt_show_output = 0;
my $opt_show_premises = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_method = 'syntactically';
my $opt_model_finder_timeout = 5;
my $opt_proof_finder_timeout = 30;
my $opt_force = 0;
my $opt_semantically_use_all_axioms = 0;

sub print_formula_names_with_color {
    my $formulas_ref = shift;
    my $color = shift;

    my @formulas = @{$formulas_ref};

    if (scalar @formulas == 0) {
	say '(none)';
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
	'method=s' => \$opt_method,
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
    my $conjecture = $theory->get_conjecture ();

    if (scalar @axioms == 0) {
	print colored ('Step 1', 'blue'), ': Derive the conjecture:';
    } elsif (scalar @axioms == 1) {
	print colored ('Step 1', 'blue'), ': Derive the conjecture the sole available premises:';
    } else {
	print colored ('Step 1', 'blue'), ': Derive the conjecture from all ', scalar @axioms, ' available premises:';
    }

    my $initial_proof_result = TPTP::prove ($theory);
    my $initial_proof_szs_status
	= $initial_proof_result->has_szs_status ? $initial_proof_result->get_szs_status
	    : 'Unknown';

    if ($initial_proof_szs_status eq 'Theorem') {
	say $SPACE, colored ('OK', 'green');
    } else {
	say $SPACE, colored ('Not OK', 'red'), ' (SZS status ', $initial_proof_szs_status, ')';
    }

    if ($initial_proof_szs_status ne 'Theorem' && $opt_force) {
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

    if ($initial_proof_szs_status ne 'Theorem' && ! $opt_force) {
	return 1;
    }

    if (! $opt_semantically_use_all_axioms) {
	my $derivation = $initial_proof_result->output_as_derivation ();
	my @unused_formulas = $derivation->get_unused_premises ();
	warn 'There were ', scalar @unused_formulas, ' unused formulas.';
	foreach my $unused (@unused_formulas) {
	    $theory = $theory->remove_formula ($unused);
	}
	@axioms = $theory->get_axioms ();
    }

    my @axioms_now = $theory->get_axioms (1);
    warn 'After deleting unused formulas, the theory now has these axioms: ', Dumper (map { $_->get_name () } @axioms_now);

    $theory = $theory->promote_conjecture_to_false_axiom ();

    my %needed = ();
    my %unneeded = ();
    my %unknown = ();

    say colored ('Step 2', 'blue'), ': Determine needed premises (deletion leads to countersatisfiability):';

    print 'PREMISES (', colored ('needed', $NEEDED_PREMISE_COLOR), ' / ', colored ('not needed', $UNNEEDED_PREMISE_COLOR), ' / ', colored ('unknown', $UNKNOWN_COLOR), ')', "\N{LF}";

    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	my $trimmed_theory = $theory->remove_formula ($axiom);
	my $tptp_result = eval
	    { TPTP::find_model ($trimmed_theory, { 'timeout' => $opt_model_finder_timeout }) };
	my $tptp_find_model_message = $@;

	my $szs_status
	    = (defined $tptp_result && $tptp_result->has_szs_status ()) ? $tptp_result->get_szs_status () : 'Unknown';

	# DEBUG
	# warn 'SZS status for ', $axiom_name, ': ', $szs_status;

	if (defined $tptp_result) {
	    if ($tptp_result->timed_out ()) {
		say colored ($axiom_name, $UNKNOWN_COLOR);
		$unknown{$axiom_name} = 0;
	    } elsif ($szs_status eq 'Satisfiable') {
		say colored ($axiom_name, $NEEDED_PREMISE_COLOR);
		$needed{$axiom_name} = 0;
	    } elsif ($szs_status eq 'Unsatisfiable') {
		say colored ($axiom_name, $UNNEEDED_PREMISE_COLOR);
		$unneeded{$axiom_name} = 0;
	    } else {
		say colored ($axiom_name, $UNKNOWN_COLOR);
		$unknown{$axiom_name} = 0;
	    }
	} else {
	    say {*STDERR} warning_message ('Something went wrong when testing whether ', $axiom_name, ' can be removed:', "\N{LF}", $tptp_find_model_message);
	}

    }

    print colored ('Step 3', 'blue'), ': Try to derive the conjecture from only ', colored ('needed', $NEEDED_PREMISE_COLOR), ' premises:';

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
    $small_theory = $small_theory->remove_formula ($conjecture);
    $small_theory = $small_theory->add_formula ($conjecture);

    if (! $small_theory->has_conjecture_formula ()) {
	my $small_theory_path = $small_theory->get_path ();
	croak 'The theory ', $small_theory_path, ' has no conjecture formula!';
    }

    my $new_result = TPTP::prove ($small_theory,
				  { 'timeout' => $opt_proof_finder_timeout });
    my $new_result_szs_status
	= $new_result->has_szs_status () ? $new_result->get_szs_status () : 'Unknown';

    if ($new_result_szs_status eq 'Theorem') {
	say $SPACE, colored ('OK', 'green');
    } else {
	say $SPACE, colored ('Not OK', 'red'), ' (SZS status ', $new_result_szs_status, ')';
    }

    if ($new_result_szs_status eq 'Theorem') {
	say 'The needed formulas alone suffice to prove the conjecture.';
    } else {
	say 'The needed formulas alone do not suffice to prove the conjecture.';
	say 'We will now try, for each premise P that was marked as ', colored ('unknown', $UNKNOWN_COLOR), ',';
	say 'to derive the conjecture from P plus the premises marked ', colored ('needed', $NEEDED_PREMISE_COLOR), '.';

	say 'UNKNOWN PREMISE';

	my %still_unknown = ();
	my %known_now = ();

	my @unknown_formula_names = keys %unknown;
	foreach my $formula_name (@unknown_formula_names) {
	    my $formula = $theory->formula_with_name ($formula_name);
	    my $bigger_theory = $small_theory->add_formula ($formula);
	    my $bigger_result = TPTP::prove ($bigger_theory,
					     { 'timeout' => $opt_proof_finder_timeout });
	    my $bigger_result_szs_status
		= $bigger_result->has_szs_status () ?
		    $bigger_result->get_szs_status ()
			: 'Unknown';
	    if ($bigger_result_szs_status eq 'Theorem') {
		say colored ($formula_name, $GOOD_COLOR);
		$known_now{$formula_name} = 0;
	    } else {
		say colored ($formula_name, $UNKNOWN_COLOR), ' (SZS status ', $bigger_result_szs_status, ')';
		$still_unknown{$formula_name} = 0;
	    }
	}

	if (scalar keys %known_now == 0) {
	    say 'We were unfortunately not able to determine whether any ', colored ('unknown', $UNKNOWN_COLOR), ' premise,';
	    say 'together with all ', colored ('needed', $NEEDED_PREMISE_COLOR), ' premises, suffices to derive the conjecture.';
	} elsif (scalar keys %still_unknown) {
	    say 'We were able to determine, for each of the following premises P,';
	    say 'that the ', colored ('needed', $NEEDED_PREMISE_COLOR), ' premises plus P suffice to derive the conjecture:';
	    foreach my $formula_name (sort keys %known_now) {
		say colored ($formula_name, $USED_PREMISE_COLOR);
	    }
	    say 'However, we were unable to make a similar decision for the following premises:';
	    foreach my $formula_name (sort keys %still_unknown) {
		say colored ($formula_name, $UNKNOWN_COLOR);
	    }
	} else {

	}

    }

    return 1;

}

1;
__END__
