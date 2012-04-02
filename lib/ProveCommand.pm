package ProveCommand;

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
use Regexp::DefaultFlags;
use feature 'say';

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory);
use Utils qw(error_message
	     ensure_readable_file);

Readonly my $STDIN_PATH => q{--};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $EMPTY_STRING => q{};
Readonly my $DESCRIPTION => 'Try proving a conjecture.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'yellow';

my $opt_show_output = 0;
my $opt_show_premises = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_solution_szs_status = 'Theorem';

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

sub BUILD {
    my $self = shift;
    $self->_set_description ($DESCRIPTION);
    return $self;
}

around 'execute' => sub {
    my $orig = shift;
    my $self = shift;
    my @arguments = @_;

    GetOptionsFromArray (
	\@arguments,
	'show-output' => \$opt_show_output,
	'show-premises' => \$opt_show_premises,
	'man' => \$opt_man,
	'verbose' => \$opt_verbose,
	'help|?' => \$opt_help,
	'solution-szs-status=s' => \$opt_solution_szs_status,
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

    if ($opt_solution_szs_status eq $EMPTY_STRING) {
	pod2usage (-msg => error_message ('The empty string is not an acceptable SZS problem status.'),
		   -exitval => 2);

    }

    if ($opt_solution_szs_status !~ /\A [A-Za-z]+ \z/) {
	pod2usage (-msg => error_message ('Unacceptable SZS problem status', "\N{LF}", "\N{LF}", $TWO_SPACES, $opt_solution_szs_status),
		   -exitval => 2);
    }

    if (scalar @arguments == 0) {
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2);
    }

    if (scalar @arguments > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the prove arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
		   -exitval => 2);
    }

    if (! ensure_tptp4x_available ()) {
	say STDERR error_message ('Cannot run tptp4X');
	exit 1;
    }

    my $theory_path = $arguments[0];

    if (! ensure_readable_file ($theory_path)) {
	say {*STDERR} error_message ('The file at', $SPACE, $theory_path, $SPACE, 'does not exist (or is unreadable).');
	exit 1;
    }

    if (ensure_sensible_tptp_theory ($theory_path)) {
	return $self->$orig (@arguments);
    } else {
	say STDERR error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	exit 1;
    }

    return;

};

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $theory_path = $arguments[0];

    my $theory = Theory->new (path => $theory_path);

    my $result = TPTP::prove ($theory);

    if (! $result->exited_cleanly ()) {
	my $exit_code = $result->get_exit_code ();
	my $err_output = $result->get_error_output ();
	say STDERR (error_message ('The prover terminated, but it did not exit cleanly when working with ', $theory_path, '.'));
	say STDERR 'The exit code was', $SPACE, $exit_code, ', and the error output was:';
	say $err_output;
	exit 1;
    }

    my $szs_status = $result->has_szs_status () ? $result->get_szs_status () : 'Unknown';

    my $ok = ($szs_status eq $opt_solution_szs_status);
    say colored ($szs_status, $ok ? $GOOD_COLOR : $BAD_COLOR);



    if ($opt_debug) {
	print "\N{LF}", 'The prover output was:', "\N{LF}";
	print $result->get_output ();
    }

    if ($ok && $opt_show_premises) {
	my $derivation = $result->output_as_derivation ();
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

    }

    return 1;

}

1;
__END__
