package ModelCommand;

require v5.10; # for the 'say' feature
use feature 'say';

use Moose;
use Carp qw(croak carp);
use Pod::Find qw(pod_where);
use Pod::Usage;
use Readonly;
use Getopt::Long qw(GetOptionsFromArray :config gnu_compat);
use charnames qw(:full);
use English qw(-no_match_vars);
use Data::Dumper;
use Term::ANSIColor qw(colored);

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    known_prover);
use Utils qw(error_message
	     asterisk_list
	     ensure_readable_file);
use SZS qw(is_szs_success
	   szs_implies);

Readonly my $EMPTY_STRING => q{};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $LF => "\N{LF}";
Readonly my $FULL_STOP => q{.};
Readonly my $DESCRIPTION => 'Find models of a theory.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';
Readonly my $SZS_SATISFIABLE => 'Satisfiable';

my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_with_conjecture_as = undef;
my $opt_timeout = 30; # seconds
my $opt_model_finder = 'paradox';

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
	'timeout=i' => \$opt_timeout,
	'model-finder=s' => \$opt_model_finder,
    ) or pod2usage (
	-exitval => 2,
	-input => pod_where({-inc => 1}, __PACKAGE__),
    );

    if ($opt_help) {
        pod2usage(
	    -exitval => 1,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if ($opt_man) {
        pod2usage(
            -exitstatus => 0,
            -verbose    => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
        );
    }

    # debug implies verbose
    if ($opt_debug) {
        $opt_verbose = 1;
    }

    if (scalar @arguments == 0) {
	pod2usage (
	    -msg => error_message ('Please supply a TPTP theory file.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if (scalar @arguments > 1) {
	pod2usage (
	    -msg => error_message ('Unable to make sense of the prove arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__));
    }

    if (! ensure_tptp4x_available ()) {
	say STDERR error_message ('Cannot run tptp4X');
	exit 1;
    }

    if (! known_prover ($opt_model_finder)) {
	say {*STDERR} error_message ('Unknown model finder', $SPACE, $opt_model_finder, $FULL_STOP);

	my @supported_provers = supported_provers ();
	say {*STDERR} 'The following provers are known:', "\N{LF}";
	if (scalar @supported_provers == 0) {
	    say {*STDERR} '(none)';
	} else {
	    say asterisk_list (@supported_provers);
	}

	exit 1;
    }

    my $theory_path = $arguments[0];

    if (! ensure_readable_file ($theory_path)) {
	say {*STDERR} error_message ('There is no file at', $SPACE, $theory_path, $SPACE, '(or it is unreadable).');
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

    # Transform the theory as requested, if it has a conjecture
    # formula.
    if ($theory->has_conjecture_formula ()) {
	if (defined $opt_with_conjecture_as) {
	    if ($opt_with_conjecture_as eq 'true') {
		$theory = $theory->promote_conjecture_to_true_axiom ();
	    } else {
		$theory = $theory->promote_conjecture_to_false_axiom ();
	    }
	} else {
	    # $theory = $theory->strip_conjecture ();
	}
    }

    my $result = TPTP::prove ($theory,
			      $opt_model_finder,
			      { 'timeout' => $opt_timeout });

    if ($result->timed_out ()) {
	say colored ('Timeout', $UNKNOWN_COLOR);
	exit 1;
    }

    if (! $result->exited_cleanly ()) {
	say colored ('Error', $BAD_COLOR), $SPACE, '(unclean exit; we may have killed it ourselves)';
	exit 1;
    }

    my $model = eval { $result->output_as_model () };
    my $model_message = $@;

    if (! defined $model) {
	my $output = $result->get_output ();
	say {*STDERR} error_message ('Something went wrong interpreting the output as a model', $theory_path, ': ', $model_message);
	if (defined $output && $output ne $EMPTY_STRING) {
	    say {*STDERR} 'The output we tried to interpret was:', $LF;
	    say {*STDERR} $output;
	} else {
	    say {*STDERR} 'Strangely, there was no output at all.  (So how could we possibly interpret it as a model?)';
	}

	exit 1;
    }

    my $szs_status = $result->get_szs_status ();

    if (is_szs_success ($szs_status)) {
	my $model_description = $model->describe ();
	if (defined $model_description) {
	    say $model_description;
	} else {
	    say {*STDERR} error_message ('Although', $SPACE, $opt_model_finder, $SPACE, 'terminated cleanly and gives the SZS status');
	    say $szs_status, ', we failed to extract a description of a model.';
	    exit 1;
	}
    } else {
	say colored ($szs_status, $BAD_COLOR);
    }

    return 1;

}

1;
__END__

=pod

=head1 NAME

tipi model

=head1 SYNOPSIS

tipi model --help

tipi model --man

tipi model [--timeout=N] TPTP-file

=head1 DESCRIPTION

Given a TPTP problem file, attempt to build a model of the axioms of
the TPTP problem.

At most --timeout seconds are spent searching for such an
interpretation (by default, the timeout is 30 seconds).

=head1 SEE ALSO

=over 8

=item L<The SZS Ontology|http://www.cs.miami.edu/~tptp/cgi-bin/SeeTPTP?Category=Documents&File=SZSOntology>

=back

=cut
