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
use feature 'say';

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available ensure_valid_tptp_file prove_if_possible ensure_sensible_tptp_theory);
use Utils qw(error_message);

Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $DESCRIPTION => 'Try proving a conjecture.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';

my $opt_show_output = 0;
my $opt_show_premises = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;

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
	'help|?' => => \$opt_help,
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

    my $result = TPTP::prove ($theory);

    if (! $result->exited_cleanly ()) {
	say STDERR (error_message ('The prover terminated, but it did not exit cleanly when working with ', $theory_path, '.'));
	exit 1;
    }

    my $szs_status = $result->get_szs_status ();

    if (defined $szs_status) {

	if ($szs_status eq 'Theorem') {
	    my $derivation = $result->output_as_derivation ();

	    print 'SZS Status: ', colored ($szs_status, $GOOD_COLOR), "\N{LF}";

	    if ($opt_show_output) {
		print "\N{LF}", 'The prover output:', "\N{LF}";
		print $result->get_output ();
	    }

	    if ($opt_show_premises) {
		print "\N{LF}";
		report_used_and_unused_premises ($derivation);
	    }

	} elsif ($szs_status eq 'CounterSatisfiable'
	     || $szs_status eq 'CounterTheorem') {

	    print 'SZS Status: ', colored ($szs_status, $BAD_COLOR), "\N{LF}";

	    if ($opt_show_output) {
		print "\N{LF}", 'The prover output:', "\N{LF}";
		print $result->get_output ();
	    }

	}

    } else {

	print {*STDERR} 'Error: no SZS status for this prover run for ', $theory_path, '.', "\N{LF}";
	print {*STDERR} 'The prover output was:', "\N{LF}";
	print {*STDERR} $result->get_output ();
	exit 1;

    }

    return 1;

}

1;
__END__
