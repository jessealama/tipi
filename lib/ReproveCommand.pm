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

Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'bright_black';
Readonly my $DESCRIPTION => 'Prove a conjecture again, perhaps using different premises.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';

my $opt_show_output = 0;
my $opt_show_premises = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_method = 'syntactically';
my $opt_timeout = 5;

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
	'timeout=i' => \$opt_timeout,
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

    if ($opt_timeout < 0) {
	pod2usage (-msg => error_message ('Invalid value ', $opt_timeout, ' for the timeout option.'),
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
    my $proof_result = eval { TPTP::prove ($theory) };

    if (! defined $proof_result) {
	print {*STDERR} message (warning_message ('We could not verify that the conjecture of ', $theory_path, ' really is a consequence of its axioms.'));
	print {*STDERR} message ('The results that follow may be meaningless.');
    }

    my $proof_szs_status
	= $proof_result->has_szs_status ? $proof_result->get_szs_status : 'Unknown';

    my @axioms = $theory->get_axioms ();

    $theory = $theory->promote_conjecture_to_false_axiom ();

    my %needed = ();

    print 'PREMISES (', colored ('needed', $USED_PREMISE_COLOR), ' / ', colored ('not needed', $UNUSED_PREMISE_COLOR), ' / ', colored ('unknown', $UNKNOWN_COLOR), ')', "\N{LF}";

    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	my $trimmed_theory = $theory->remove_formula ($axiom);
	my $tptp_result = eval
	    { TPTP::find_model ($trimmed_theory, { 'timeout' => $opt_timeout }) };
	my $tptp_find_model_message = $@;

	my $szs_status
	    = (defined $tptp_result && $tptp_result->has_szs_status ()) ? $tptp_result->get_szs_status () : 'Unknown';

	# DEBUG
	# warn 'SZS status for ', $axiom_name, ': ', $szs_status;

	if (defined $tptp_result) {
	    if ($tptp_result->timed_out ()) {
		print colored ($axiom_name, $UNKNOWN_COLOR);
	    } elsif ($szs_status eq 'Satisfiable') {
		print colored ($axiom_name, $USED_PREMISE_COLOR);
	    } elsif ($szs_status eq 'Unsatisfiable') {
		print colored ($axiom_name, $UNUSED_PREMISE_COLOR);
	    } else {
		print colored ($axiom_name, $UNKNOWN_COLOR);
	    }
	} else {
	    print {*STDERR} message (warning_message ('Something went wrong when testing whether ', $axiom_name, ' can be removed:', "\N{LF}", $tptp_find_model_message));
	}

	print "\N{LF}";
    }

    return 1;

}

1;
__END__
