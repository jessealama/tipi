package IndependenceCommand;

require v5.10.0;

use Moose;
use Carp qw(croak carp);
use Pod::Usage;
use Readonly;
use Getopt::Long qw(GetOptionsFromArray :config gnu_compat);
use charnames qw(:full);
use English qw(-no_match_vars);
use Data::Dumper;
use Term::ANSIColor qw(colored);
use List::MoreUtils qw(any first_index);
use feature 'say';

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory);
use Utils qw(error_message
	     ensure_readable_file
	     warning_message);

Readonly my $COLON => q{:};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $DESCRIPTION => 'Try proving a conjecture.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';

my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_thorough = 0;

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
	'thorough' => \$opt_thorough,
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

    if ($opt_verbose && $theory->has_conjecture_formula ()) {
	say {*STDERR} warning_message ('The theory at', $SPACE, $theory_path, $SPACE, 'has a conjecture formula; for checking independence, it will be ignored.');
    }

    $theory = $theory->strip_conjecture ();

    my @axioms = $theory->get_axioms (1);

    if (any { ! defined $_ } @axioms) {
	croak 'We found an undefined axiom.';
    }

    my %known_dependent = ();
    my %unknown = ();

  INDEPENDENCE:
    foreach my $i (0 .. scalar @axioms - 1) {
	my $axiom = $axioms[$i];
	my $axiom_name = $axiom->get_name ();
	my $independence = $theory->independent_axiom ($axiom);

	if ($opt_debug) {
	    say {*STDERR} 'Testing indepedence of', $SPACE, $axiom_name, $COLON, $SPACE, $independence;
	}

	if ($independence == 1) {
	    # nothing to do but keep going
	} elsif ($independence == 0) {
	    $known_dependent{$axiom_name} = 0;
	    if (! $opt_thorough) {
		last INDEPENDENCE;
	    }
	} else {
	    $unknown{$axiom_name} = 0;
	    if (! $opt_thorough) {
		last INDEPENDENCE;
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
	say join ("\N{LF}", @dependent_colored);
	exit 1;
    } elsif (scalar @dependent == 0) {
	say colored ('Unknown', $UNKNOWN_COLOR);
	say 'No axiom was shown to be derivable from the other axioms,';
	say 'but for each the following axioms we were unable to determine';
	say 'whether it is underivable from the other axioms:';
	say join ("\N{LF}", @unknown_colored);
	exit 1;
    } else {
	say colored ('Dependent', $BAD_COLOR);
	say 'The following have been shown to be derivable from the other axioms:';
	say join ("\N{LF}", @dependent_colored);
	say 'Moreover, for each the following axioms we were unable to determine';
	say 'whether it is underivable from the other axioms:';
	say join ("\N{LF}", @unknown_colored);
	exit 1;
    }

}

1;
__END__
