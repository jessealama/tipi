package ParseCommand;

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
use feature 'say';
use List::Util qw(max);
use POSIX qw(floor);

extends 'Command';

use Formula qw(parse_tptp_file parse_tptp_formula parse_fof_formula);
use Utils qw(error_message is_readable_file);

Readonly my $DESCRIPTION => 'Parse a TPTP file.';
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };


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
	'man' => \$opt_man,
	'verbose' => \$opt_verbose,
	'help|?' => \$opt_help,
	'debug' => \$opt_debug,
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
	    -msg => error_message ('Unable to make sense of the parse arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
);
    }

    return $self->$orig (@arguments);

};

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $arg = $arguments[0];
    my $parsed = is_readable_file ($arg) ? parse_tptp_file ($arg) : parse_tptp_formula ($arg);

    if (defined $parsed) {
        say $parsed;
        return 1;
    } else {
        confess $arg, ' could not be parsed.';
        return 0;
    }
}

1;
__END__

=pod

=head1 NAME

tipi parse

=head1 SYNOPSIS

tipi parse [ --help | --man ]

tipi symbols TPTP-file

=head1 DESCRIPTION

B<tipi symbols> takes a TPTP file and gives information about the
predicate and function symbols appearing in it.

If a predicate or function occurs exactly once, its name will be
printed in red.  This is often (but not always) an indication that
something is wrong with the theory (did you misspell something?).

=cut
