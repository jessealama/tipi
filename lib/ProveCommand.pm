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
	    ensure_sensible_tptp_theory
	    known_prover
	    tptp4X_output);
use Utils qw(error_message
	     ensure_readable_file
	     print_formula_names_with_color);
use SZS qw(is_szs_success
	   szs_implies
	   known_szs_status);

Readonly my $STDIN_PATH => q{--};
Readonly my $TWO_SPACES => q{  };
Readonly my $FULL_STOP => q{.};
Readonly my $SPACE => q{ };
Readonly my $LF => "\N{LF}";
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
my $opt_prover = 'eprover';
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
	'show-output' => \$opt_show_output,
	'show-premises' => \$opt_show_premises,
	'man' => \$opt_man,
	'verbose' => \$opt_verbose,
	'debug' => \$opt_debug,
	'help|?' => \$opt_help,
	'solution-szs-status=s' => \$opt_solution_szs_status,
	'with-prover=s' => \$opt_prover,
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

    if ($opt_timeout < 0) {
	pod2usage (-msg => error_message ($opt_timeout, $SPACE, 'is an inappropriate value for the prover timeout.'),
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

    if (! known_prover ($opt_prover)) {
	say {*STDERR} error_message ($opt_prover, $SPACE, 'is not a known theorem prover.');
	exit 1;
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

    if ($opt_solution_szs_status eq $EMPTY_STRING) {
	say {*STDERR} error_message ('The empty string is not an acceptable value for the intended SZS status.');
	exit 1;
    }

    if (! known_szs_status ($opt_solution_szs_status)) {
	say {*STDERR} error_message ('\'', $opt_solution_szs_status, '\' is not a known SZS status.');
	exit 1;
    }

    my $theory_path = $arguments[0];

    if (! ensure_readable_file ($theory_path)) {
	say {*STDERR} error_message ('The file at', $SPACE, $theory_path, $SPACE, 'does not exist (or is unreadable).');
	exit 1;
    }

    if (! ensure_sensible_tptp_theory ($theory_path)) {
	my $errors = tptp4X_output ($theory_path);
	say {*STDERR} error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	say {*STDERR} 'Here is what tptp4X said:';
	say {*STDERR} $errors;
	exit 1;
    }

    return $self->$orig (@arguments);

};

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $theory_path = $arguments[0];
    my $theory = Theory->new (path => $theory_path);
    my $result = TPTP::prove ($theory,
			      $opt_prover,
			      { 'timeout' => $opt_timeout });

    if ($opt_debug) {
	warn 'We got the following result:', $LF, Dumper ($result);
    }

    my $err_output = $result->get_error_output ();
    my $std_output = $result->get_output ();

    my $szs_status = $result->get_szs_status ();

    if (is_szs_success ($szs_status)) {
	if (szs_implies ($szs_status, $opt_solution_szs_status)) {
	    say colored ($szs_status, $GOOD_COLOR);
	} else {
	    say colored ($szs_status, $BAD_COLOR);
	}

	if ($opt_show_premises) {
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

    } else {
	say colored ($szs_status, $BAD_COLOR);
    }

    return 1;

}

1;
__END__

=pod

=head1 NAME

tipi prove

=head1 SYNOPSIS

tipi prove --help

tipi prove --man

tipi prove [--verbose | --debug] [--solution-szs-status=STATUS] [--timeout=N] [--with-prover=PROVER] TPTP-file

=head1 DESCRIPTION

tipi prove simply attempts to solve a reasoning problem specified in
the TPTP file.

To say what "solve" means, use the C<--solution-szs-status> option.
If you do not specify this, the SZS status B<Theorem> is used by
default.

If the C<--timeout> option is absent, a default timeout of 30 seconds
will be used.

The theorem prover specified in the C<--with-prover> option will be used.
One can repeat this option.  The interpretation is that you are
specifying a set of theorem provers to be used to determine
(un)derivability.  If you omit specifying this option, then by
default, two provers will be used: E and Paradox.  If the
C<--with-prover> option is used, these defaults will be discarded, and
all and only the provers you specify will be used.

=head1 SEE ALSO

=over 8

=item L<The SZS Ontology|http://www.cs.miami.edu/~tptp/cgi-bin/SeeTPTP?Category=Documents&File=SZSOntology>

=back

=cut
