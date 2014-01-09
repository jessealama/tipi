package EquivalentCommand;

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
Readonly my $LLF => ($LF x 2);
Readonly my $SSP => ($SPACE x 2);
Readonly my $FLUSH => "${LLF}${SSP}";
Readonly my $DESCRIPTION => 'Are two theories equivalent?';
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

    if (scalar @arguments != 2) {
	pod2usage (-msg =>
                       error_message ('Please supply two TPTP theory files.'),
		   -exitval => 2,
		   -input => pod_where({-inc => 1}, __PACKAGE__));
    }

    if (scalar @opt_provers == 0) {
	@opt_provers = ('paradox', 'eprover');
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

    my $theory_1_path = $arguments[0];
    my $theory_2_path = $arguments[1];

    if (! ensure_readable_file ($theory_1_path)) {
	say {*STDERR} error_message ('The file at', $SPACE, $theory_1_path, $SPACE, 'does not exist (or is unreadable).');
	exit 1;
    }

    if (! ensure_readable_file ($theory_2_path)) {
	say {*STDERR} error_message ('The file at', $SPACE, $theory_2_path, $SPACE, 'does not exist (or is unreadable).');
	exit 1;
    }

    if (ensure_sensible_tptp_theory ($theory_1_path)) {
	return $self->$orig (@arguments);
    } else {
	say STDERR error_message ('The file at ', $theory_1_path, ' is not a valid TPTP file.');
	exit 1;
    }

    if (ensure_sensible_tptp_theory ($theory_2_path)) {
	return $self->$orig (@arguments);
    } else {
	say STDERR error_message ('The file at ', $theory_2_path, ' is not a valid TPTP file.');
	exit 1;
    }

    return;

};

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $theory_1_path = $arguments[0];
    my $theory_2_path = $arguments[1];
    my $theory_1 = Theory->new (path => $theory_1_path);
    my $theory_2 = Theory->new (path => $theory_2_path);

    # fofify
    $theory_1 = $theory_1->fofify ();
    $theory_2 = $theory_2->fofify ();

    if ($theory_1->has_conjecture_formula ()) {
	$theory_1 = $theory_1->strip_conjecture ();
	say {*STDERR} warning_message ('The theory at', $SPACE, $theory_1_path, $SPACE, 'has a conjecture formula.');
	say {*STDERR} 'For the purposes of checking whether it is equivalent to', $FLUSH, $theory_2_path, $LLF, 'the conjecture will be discarded so that we can focus on just the axioms of the theory.';
    }

    if ($theory_2->has_conjecture_formula ()) {
	$theory_2 = $theory_2->strip_conjecture ();
	say {*STDERR} warning_message ('The theory at', $SPACE, $theory_2_path, $SPACE, 'has a conjecture formula.');
	say {*STDERR} 'For the purposes of checking whether it is equivalent to', $FLUSH, $theory_1_path, $LLF, 'the conjecture will be discarded so that we can focus on just the axioms of the theory.';
    }

    my @axioms_1 = $theory_1->get_axioms (1);
    my @axioms_2 = $theory_2->get_axioms (1);
    my @sorted_axioms_1 = sort { $a->get_name () cmp $b->get_name () } @axioms_1;
    my @sorted_axioms_2 = sort { $a->get_name () cmp $b->get_name () } @axioms_2;

    my %known_dependent = ();
    my %unknown = ();
    my %handled = ();

    say $theory_1_path;
    foreach my $axiom_1 (@sorted_axioms_1) {
	my $axiom_1_name = $axiom_1->get_name ();
	my $axiom_1_formula = $axiom_1->get_formula ();
        my @temp_axioms = @axioms_2;
        my $new_name = 'temp';
        my $new_axiom = "fof(${new_name},conjecture,${axiom_1_formula})";
        push (@temp_axioms, $new_axiom);
        my $proves = $theory_2->proves ($new_axiom,
                                        \@opt_provers,
                                        { 'timeout' => $opt_timeout });
        if ($proves == -1) {
            say $axiom_1_name, $COLON, $SPACE, colored ('unknown', $UNKNOWN_COLOR);
        } elsif ($proves == 0) {
            say $axiom_1_name, $COLON, $SPACE, colored ('no', $BAD_COLOR);
        } else {
            say $axiom_1_name, $COLON, $SPACE, colored ('yes', $GOOD_COLOR);
        }
    }

    say $theory_1_path;
    foreach my $axiom_2 (@sorted_axioms_2) {
	my $axiom_2_name = $axiom_2->get_name ();
	my $axiom_2_formula = $axiom_2->get_formula ();
        my @temp_axioms = @axioms_2;
        my $new_name = 'temp';
        my $new_axiom = "fof(${new_name},conjecture,${axiom_2_formula})";
        push (@temp_axioms, $new_axiom);
        my $proves = $theory_1->proves ($new_axiom,
                                        \@opt_provers,
                                        { 'timeout' => $opt_timeout });
        if ($proves == -1) {
            say $axiom_2_name, $COLON, $SPACE, colored ('unknown', $UNKNOWN_COLOR);
        } elsif ($proves == 0) {
            say $axiom_2_name, $COLON, $SPACE, colored ('no', $BAD_COLOR);
        } else {
            say $axiom_2_name, $COLON, $SPACE, colored ('yes', $GOOD_COLOR);
        }
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
