package CountermodelCommand;

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
Readonly my $DESCRIPTION => 'Refute a conjecture.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';

my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_show_model = 0;
my $opt_timeout = 30; # seconds

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
	'man' => \$opt_man,
	'verbose' => \$opt_verbose,
	'help|?' => => \$opt_help,
	'show-model' => \$opt_show_model,
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

    if (! ensure_tptp4x_available ()) {
	say STDERR error_message ('Cannot run tptp4X');
	exit 1;
    }

    my $theory_path = $arguments[0];

    if (! ensure_sensible_tptp_theory ($theory_path)) {
	say STDERR error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	exit 1;
    }

    my $theory = Theory->new (path => $theory_path);

    if (! $theory->has_conjecture_formula ()) {
	say {*STDERR} error_message ('The theory at', $SPACE, $theory_path, $SPACE, 'lacks a conjecture formula.');
    }

    return $self->$orig (@arguments);

};

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $theory_path = $arguments[0];

    my $theory = Theory->new (path => $theory_path);

    $theory = $theory->promote_conjecture_to_false_axiom ();

    my $result = eval { TPTP::find_model ($theory,
					  { 'timeout' => $opt_timeout }) };
    if ($result->timed_out ()) {
	say colored ('Timeout', $UNKNOWN_COLOR);
	return 1;
    }

    my $szs_status = $result->has_szs_status () ? $result->get_szs_status () : 'Unknown';
    my $exit_code = $result->get_exit_code ();

    if ($szs_status eq 'Satisfiable') {
	say colored ('CounterSatisfiable', $BAD_COLOR);
    } elsif ($exit_code == 0) {
	say colored ($szs_status, $UNKNOWN_COLOR);
    } else {
	say colored ('Error', $BAD_COLOR), $SPACE, '(The model finder did not terminate cleanly; the exit code was', $SPACE, $exit_code, '.)';
	return 1;
    }

    if ($opt_show_model) {

	my $model = eval { $result->output_as_model () };
	my $model_message = $@;

	if (! defined $model) {
	    my $output = $result->get_output ();
	    say {*STDERR} error_message ('Something went wrong interpreting the output as a model of', $SPACE, $theory_path, ': ', $model_message);
	    if (defined $output && $output ne $EMPTY_STRING) {
	    	say {*STDERR} 'The output we tried to interpret was:', "\N{LF}";
		say {*STDERR} $output;
	    } else {
		print {*STDERR} message ('Strangely, there was no output at all.  (So how could we possibly interpret it as a model?)');
	    }

	    return 0;

	}

	my $model_description = $model->describe ();
	print $model_description;

    }

    return 1;

}

1;
__END__
