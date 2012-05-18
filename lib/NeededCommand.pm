package NeededCommand;

require v5.10;

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
use Regexp::DefaultFlags;
use feature 'say';

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    known_prover
	    incompatible_szs_statuses
	    tptp4X_output);
use Utils qw(error_message
	     ensure_readable_file);

Readonly my $STDIN_PATH => q{--};
Readonly my $TWO_SPACES => q{  };
Readonly my $FULL_STOP => q{.};
Readonly my $SPACE => q{ };
Readonly my $EMPTY_STRING => q{};
Readonly my $DESCRIPTION => 'Determine whethe a premise is needed.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'yellow';
Readonly my $UNKNOWN_PREMISE_COLOR => 'yellow';
Readonly my $NEEDED_PREMISE_COLOR => 'red';
Readonly my $UNNEEDED_PREMISE_COLOR => 'cyan';

my $opt_show_output = 0;
my $opt_show_premises = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_solution_szs_status = 'Theorem';
my $opt_timeout = 30;
my $opt_prover = 'eprover';
my $opt_model_finder = 'paradox';
my $opt_syntactically = 0;
my $opt_semantically = 0;

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
	'prover=s' => \$opt_prover,
	'timeout=i' => \$opt_timeout,
	'semantically' => \$opt_semantically,
	'syntactically' => \$opt_syntactically,
    ) or pod2usage (
	-exitval => 2,
	-input => pod_where({-inc => 1}, __PACKAGE__)
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

    if ($opt_solution_szs_status eq $EMPTY_STRING) {
	pod2usage (
	    -msg => error_message ('The empty string is not an acceptable SZS problem status.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if ($opt_solution_szs_status !~ /\A [A-Za-z]+ \z/) {
	pod2usage (
	    -msg => error_message ('Unacceptable SZS problem status', "\N{LF}", "\N{LF}", $TWO_SPACES, $opt_solution_szs_status),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if ($opt_timeout < 0) {
	say {*STDERR} error_message ('Unacceptable value for the timeout option:', $SPACE, $opt_timeout, $FULL_STOP);
    }

    # if both --semantically and --syntactically are unspecified, set
    # both true
    if (! $opt_semantically && ! $opt_syntactically) {
	$opt_semantically = 1;
	$opt_syntactically = 1;
    }

    if (! known_prover ($opt_prover)) {
	say {*STDERR} error_message ($opt_prover, $SPACE, 'is not a known theorem prover.');
	exit 1;
    }

    if (scalar @arguments == 0) {
	pod2usage (
	    -msg => error_message ('Please supply a premise name and a TPTP theory file (in that order).'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if (scalar @arguments != 2) {
	pod2usage (
	    -msg => error_message ('Unable to make sense of the arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if (! ensure_tptp4x_available ()) {
	say STDERR error_message ('Cannot run tptp4X');
	exit 1;
    }

    my $premise_name = $arguments[0];

    if ($premise_name eq $EMPTY_STRING) {
	say {*STDERR} error_message ('The empty string is not the name of a premise.');
	exit 1;
    }

    my $theory_path = $arguments[1];

    if (! ensure_readable_file ($theory_path)) {
	say {*STDERR} error_message ('The file at', $SPACE, $theory_path, $SPACE, 'does not exist (or is unreadable).');
	exit 1;
    }

    if (! ensure_sensible_tptp_theory ($theory_path)) {
	my $errors = tptp4X_output ($theory_path);
	say {*STDERR} error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	say {*STDERR} 'Here is what tptp4X output when evaluating the file:';
	say {*STDERR} $errors;
	exit 1;
    }


    my $theory = Theory->new (path => $theory_path);

    if (! $theory->has_premise_with_name ($premise_name)) {
	say {*STDERR} error_message ('There is no premise of', $SPACE, $theory_path, $SPACE, 'with the name', "\N{LF}", "\N{LF}", $TWO_SPACES, $premise_name, "\N{LF}");
	say {*STDERR} 'Try', "\N{LF}", "\N{LF}", $TWO_SPACES, 'tipi premises --names-only', $SPACE, $theory_path, "\N{LF}", "\N{LF}", 'to list the premises of the theory by name.';
	exit 1;
    }

    return $self->$orig (@arguments);

};

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $premise_name = $arguments[0];
    my $theory_path = $arguments[1];

    my $theory = Theory->new (path => $theory_path);

    $theory = $theory->remove_formula_by_name ($premise_name);

    if ($opt_syntactically) {

	my $prover_result = TPTP::prove ($theory,
					 $opt_prover,
					 $opt_solution_szs_status,
					 { 'timeout' => $opt_timeout });


	my $prover_szs_status
	    = $prover_result->has_szs_status () ? $prover_result->get_szs_status () : 'Unknown';

	if ($prover_szs_status eq 'Unknown') {

	    if ($opt_semantically) {
		my $model_result = TPTP::find_model ($theory,
						     { 'timeout' => $opt_timeout });

		my $model_szs_status = $model_result->has_szs_status () ?
		    $model_result->get_szs_status ()
			: 'Unknown';

		if ($model_szs_status eq 'Unknown') {
		    say colored ('Unknown', $UNKNOWN_PREMISE_COLOR), $SPACE, '(SZS status ', $model_szs_status, ')';
		} elsif ($model_szs_status eq $opt_solution_szs_status) {
		    say colored ('Unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, '(SZS status ', $model_szs_status, ')';
		} else {
		    say colored ('Needed', $NEEDED_PREMISE_COLOR), $SPACE, '(SZS status ', $model_szs_status, ')';
		}
	    } else {
		say colored ('Unknown', $UNKNOWN_PREMISE_COLOR), $SPACE, '(SZS status ', $prover_szs_status, ')';
	    }

	} elsif ($prover_szs_status eq $opt_solution_szs_status) {
	    say colored ('Unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, '(SZS status ', $prover_szs_status, ')';
	} elsif (incompatible_szs_statuses ($prover_szs_status,
					    $opt_solution_szs_status)) {
	    say colored ('Needed', $NEEDED_PREMISE_COLOR), $SPACE, '(SZS status ', $prover_szs_status, ')';
	} else {
	    say colored ('Unknown', $UNKNOWN_PREMISE_COLOR), $SPACE, '(SZS status ', $prover_szs_status, ')';
	}
    } else {

	my $model_result = TPTP::find_model ($theory,
					     { 'timeout' => $opt_timeout });

	my $model_szs_status = $model_result->has_szs_status () ?
	    $model_result->get_szs_status ()
		: 'Unknown';

	if ($model_szs_status eq 'Unknown') {
	    say colored ('Unknown', $UNKNOWN_PREMISE_COLOR), $SPACE, '(SZS status ', $model_szs_status, ')';
	} elsif ($model_szs_status eq $opt_solution_szs_status) {
	    say colored ('Unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, '(SZS status ', $model_szs_status, ')';
	} else {
	    say colored ('Needed', $NEEDED_PREMISE_COLOR), $SPACE, '(SZS status ', $model_szs_status, ')';
	}

    }

    return 1;

}

1;
__END__

=pod

=head1 NAME

tipi needed

=head1 SYNOPSIS

tipi needed --help

tipi needed --man

tipi needed [--solution-szs-status=STATUS] [--with-prover] [--timeout=N] premise-name TPTP-file

=head1 DESCRIPTION

B<tipi needed> attempts to determine whether a given premise is needed
for a TPTP problem to be successful.

To say what "successful" means, use the C<--solution-szs-status>
option.  If you do not specify this, the SZS status B<Theorem> is
used.

If the C<--timeout> option is absent, a default timeout of 30 seconds
will be used.

The theorem prover specified in the C<--with-prover> option will be used.
One can repeat this option.  The interpretation is that you are
specifying a set of theorem provers to be used to determine
(un)derivability.  If you omit specifying this option, then by
default, two provers will be used: E and Paradox.  If the
C<--with-prover> option is used, these defaults will be discarded, and
all and only the provers you specify will be used.

It is an error if the given premise is not actually the name of a
formula in the given TPTP file.

=head1 SEE ALSO

=over 8

=item L<The SZS Ontology|http://www.cs.miami.edu/~tptp/cgi-bin/SeeTPTP?Category=Documents&File=SZSOntology>

=back


=cut
