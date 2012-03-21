package ModelCommand;

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
Readonly my $DESCRIPTION => 'Find models of a theory.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';

my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_with_conjecture_as = undef;
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
	'with-conjecture-as=s' => \$opt_with_conjecture_as,
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

    # Transform the theory
    if (defined $opt_with_conjecture_as) {
	if ($opt_with_conjecture_as eq 'true') {
	    $theory = $theory->promote_conjecture_to_true_axiom ();
	} else {
	    $theory = $theory->promote_conjecture_to_false_axiom ();
	}
    } else {
	$theory = $theory->strip_conjecture ();
    }

    my $tptp_result = eval { TPTP::find_model ($theory,
					       { 'timeout' => $opt_timeout }) };
    my $tptp_result_message = $@;

    if (! defined $tptp_result) {
	print {*STDERR} message (error_message ('Something went wrong searching for a model of ', $theory_path, '.'));
	if (defined $tptp_result_message && $tptp_result_message ne $EMPTY_STRING) {
	    	print {*STDERR} message_with_extra_linefeed ('The error was:');
		print {*STDERR} message ($tptp_result_message);
	} else {
	    print {*STDERR} message ('No further information is available.');
	}

	exit 1;
    }

    if ($tptp_result->timed_out ()) {
	print colored ('Timeout', $BAD_COLOR), "\N{LF}";
	exit 1;
    }

    my $exit_code = $tptp_result->get_exit_code ();

    if (! defined $exit_code || $exit_code != 0) {
	print colored ('Error', $BAD_COLOR);
	exit 1;
    }

    my $model = eval { $tptp_result->output_as_model () };
    my $model_message = $@;

    if (! defined $model) {
	my $tptp_output = $tptp_result->get_output ();
	print {*STDERR} message (error_message ('Something went wrong interpreting the output as a model', $theory_path, ': ', $model_message));
	if (defined $tptp_output && $tptp_output ne $EMPTY_STRING) {
	    	print {*STDERR} message_with_extra_linefeed ('The output we tried to interpret was:');
		print {*STDERR} message ($tptp_output);
	} else {
	    print {*STDERR} message ('Strangely, there was no output at all.  (So how could we possibly interpret it as a model?)');
	}

	exit 1;
    }

    my $szs_status = eval { $tptp_result->get_szs_status () };

    if (! defined $szs_status) {
	my $tptp_output = $tptp_result->get_output ();
	print {*STDERR} message (error_message ('Something went wrong extracting the SZS status for the output of a model-finding task for', $theory_path, '.'));
	if (defined $tptp_output && $tptp_output ne $EMPTY_STRING) {
	    	print {*STDERR} message_with_extra_linefeed ('The output we tried to interpret was:');
		print {*STDERR} message ($tptp_output);
	} else {
	    print {*STDERR} message ('Strangely, there was no output at all.  (So how could we possibly extract the SZS status?)');
	}

	exit 1;
    }

    print colored ($szs_status, $GOOD_COLOR), "\N{LF}"; # ... "It's all good!"

    if ($opt_show_model) {
	my $model_description = $model->describe ();
	print $model_description;
    }

    return 1;

}

1;
__END__
