package IndependenceCommand;

require v5.10.0;

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
use List::MoreUtils qw(any first_index);
use IPC::Cmd qw(can_run);
use feature 'say';

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    supported_provers
	    known_prover);
use Utils qw(error_message
	     ensure_readable_file
	     warning_message
	     asterisk_list);

Readonly my $COLON => q{:};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $LF => "\N{LF}";
Readonly my $DESCRIPTION => 'Is my set of axioms independent?';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';

my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_quick = 0;
my @opt_provers = ();
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
	'debug' => \$opt_debug,
	'verbose' => \$opt_verbose,
	'help|?' => \$opt_help,
	'quick' => \$opt_quick,
	'with-prover=s' => \@opt_provers,
	'timeout=i' => \$opt_timeout,
    ) or pod2usage (-exitval => 2,
		    -input => pod_where({-inc => 1}, __PACKAGE__));

    if ($opt_help) {
        pod2usage(-exitval => 1,
		  -input => pod_where({-inc => 1}, __PACKAGE__));
    }

    if ($opt_man) {
        pod2usage(
            -exitstatus => 0,
            -verbose    => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__)
        );
    }

    # debug implies verbose
    if ($opt_debug) {
        $opt_verbose = 1;
    }

    if (scalar @arguments == 0) {
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2,
		   -input => pod_where({-inc => 1}, __PACKAGE__));
    }

    if (scalar @arguments > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the prove arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
		   -exitval => 2,
		   -input => pod_where({-inc => 1}, __PACKAGE__));
    }

    if (scalar @opt_provers == 0) {
	@opt_provers = ('eprover', 'paradox');
    }

    my %provers = ();

    foreach my $tool (@opt_provers) {
	$provers{$tool} = 0;
    }

    @opt_provers = keys %provers;
    my @supported_provers = supported_provers ();

    foreach my $prover (@opt_provers) {
	if (known_prover ($prover)) {
	    if (can_run ($prover)) {
		next;
	    } else {
		say {*STDERR} error_message ($prover, $SPACE, 'is supported by tipi, but it could not be found.'), $LF;
		exit 1;
	    }
	} else {
	    say {*STDERR} error_message ('Unknown prover', $SPACE, $prover), $LF;
	    say {*STDERR} 'The following provers are known:', "\N{LF}";
	    if (scalar @supported_provers == 0) {
		say {*STDERR} '(none)';
	    } else {
		say asterisk_list (@supported_provers);
	    }
	    exit 1;
	}
    }

    if (! ensure_tptp4x_available ()) {
	say STDERR error_message ('Cannot run tptp4X');
	exit 1;
    }

    if (! defined $opt_timeout) {
	pod2usage (-msg => error_message ('No timeout was specfied; please supply one.'),
		   -exitval => 2,
		   -input => pod_where({-inc => 1}, __PACKAGE__));
    }

    if ($opt_timeout < 0) {
	pod2usage (-msg => error_message ('Invalid value ', $opt_timeout, ' for the --timeout option.'),
		   -exitval => 2,
		   -input => pod_where({-inc => 1}, __PACKAGE__));
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

    # fofify
    $theory = $theory->fofify ();

    if ($theory->has_conjecture_formula ()) {
	$theory = $theory->strip_conjecture ();
	say {*STDERR} warning_message ('The theory at', $SPACE, $theory_path, $SPACE, 'has a conjecture formula.');
	say {*STDERR} 'For the purposes of checking independence, it will be discarded';
	say {*STDERR} 'so that we can focus on just the axioms of the theory.';
    }

    my @axioms = $theory->get_axioms (1);
    my @sorted_axioms = sort { $a->get_name () cmp $b->get_name () } @axioms;

    my %known_dependent = ();
    my %unknown = ();

    foreach my $axiom (@sorted_axioms) {
	my $axiom_name = $axiom->get_name ();
	# warn $axiom_name;
	my $independence = $theory->independent_axiom ($axiom,
						       \@opt_provers,
						       { 'timeout' => $opt_timeout });

	if ($independence == -1) {
	    $unknown{$axiom_name} = 0;
	} elsif ($independence == 0) {
	    $known_dependent{$axiom_name} = 0;
	} elsif ($independence == 1) {
	    # keep going
	} else {
	    confess 'Unexpected value', $SPACE, $independence, $SPACE, 'from the independence-checking function.';
	}

	# if (! $opt_quick && ($independence == -1 || $independence == 0)) {
	#     last;
	# }

	if ($opt_verbose) {
	    print $axiom_name, $COLON, $SPACE;
	    if (defined $known_dependent{$axiom_name}) {
		say colored ('dependent', $BAD_COLOR);
	    } elsif (defined $unknown{$axiom_name}) {
		say colored ('unknown', $UNKNOWN_COLOR);
	    } else {
		say colored ('underivable', $GOOD_COLOR);
	    }
	}


    }

    my @dependent = sort keys %known_dependent;
    my @unknown = sort keys %unknown;

    my @dependent_colored = map { colored ($_, $BAD_COLOR) } @dependent;
    my @unknown_colored = map { colored ($_, $UNKNOWN_COLOR) } @unknown;

    if (scalar @unknown == 0 && scalar @dependent == 0) {
	say colored ('Independent', $GOOD_COLOR);
	exit 0;
    } elsif (scalar @unknown == 0) {
	say colored ('Dependent', $BAD_COLOR);
	say 'The following have been shown to be derivable from the other axioms:';
	say join ("\N{LF}", @dependent);
	exit 1;
    } elsif (scalar @dependent == 0) {
	say colored ('Unknown', $UNKNOWN_COLOR);
	say 'No axiom was shown to be derivable from the other axioms,';
	say 'but for each the following axioms we were unable to determine';
	say 'whether it is underivable from the other axioms:';
	say join ("\N{LF}", @unknown);
	exit 1;
    } else {
	say colored ('Dependent', $BAD_COLOR);
	say 'The following have been shown to be derivable from the other axioms:';
	say join ("\N{LF}", @dependent);
	say 'Moreover, for each the following axioms we were unable to determine';
	say 'whether it is underivable from the other axioms:';
	say join ("\N{LF}", @unknown);
	exit 1;
    }

}

1;
__END__

=pod

=head1 NAME

tipi independence

=head1 SYNOPSIS

tipi independence [--help | --man]

tipi independence [--quick] [--timeout=N] [--with-prover=PROVER] TPTP-file

=head1 DESCRIPTION

B<tipi independence> attempts to show that the set of axioms of the
supplied TPTP file is independent, which means that no axiom can be
derived (in classical first-order logic with identity) from the
others.

For each axiom in the theory, B<tipi independence> attempts to derive
it from the others, or show that it cannot be derived.  As each axiom
is considered in turn, B<tipi independence> prints a message
indicating its status (derivable, underivable, unknown).

The theorem prover specified in the C<--with-prover> option will be used.
One can repeat this option.  The interpretation is that you are
specifying a set of theorem provers to be used to determine
(un)derivability.  If you omit specifying this option, then by
default, two provers will be used: E and Paradox.  If the
C<--with-prover> option is used, these defaults will be discarded, and
all and only the provers you specify will be used.

If C<--quick> is supplied, B<tipi independence> will terminate as soon as
one axiom is detected that either can be derived from the other axioms
or which is not known to be underivable (given the resource limits and
the theorem prover/model finder employed).  By default, all axioms
will be considered.

If there is a conjecture formula in the given TPTP file, it will be
discarded.  (The given file will not be altered.  A temporary
conjecture-free copy will be created, and that file, rather than the
initially given one, will be worked on.)

=cut
