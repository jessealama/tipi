package ConjectureCommand;

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
use feature 'say';

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    tptp4X_output);
use Utils qw(error_message);

Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'bright_black';
Readonly my $DESCRIPTION => 'Print the conjecture(s) of a TPTP problem file.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';

my $opt_show_output = 0;
my $opt_show_premises = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_name_only = 0;

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
	'name-only' => \$opt_name_only,
    ) or pod2usage (-exitval => 2,
		    -input => pod_where({-inc => 1}, __PACKAGE__));

    if ($opt_help) {
        pod2usage(1);
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
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2,
		   -input => pod_where({-inc => 1}, __PACKAGE__));
    }

    if (scalar @arguments > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the premises arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
		   -exitval => 2,
		   -input => pod_where({-inc => 1}, __PACKAGE__));
    }

    if (! ensure_tptp4x_available ()) {
	say STDERR error_message ('Cannot run tptp4X');
	exit 1;
    }

    my $theory_path = $arguments[0];

    if (! ensure_sensible_tptp_theory ($theory_path)) {
	my $errors = tptp4X_output ($theory_path);
	say {*STDERR} error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	say {*STDERR} 'Here is what tptp4X output when evaluating the file:';
	say {*STDERR} $errors;
	exit 1;
    }

    return $self->$orig (@arguments);

};

sub execute {
    my $self = shift;
    my @arguments = shift;

    my $theory_path = $arguments[0];
    my $theory = Theory->new (path => $theory_path);

    if ($theory->has_conjecture_formula ()) {
	my $conjecture = $theory->get_conjecture ();
	if ($opt_name_only) {
	    my $conjecture_name = $conjecture->get_name ();
	    say $conjecture_name;
	} else {
	    my $conjecture_formula = $conjecture->get_formula ();
	    say $conjecture_formula;
	}
    } else {
	say STDERR 'The TPTP theory file ', $theory_path, ' has no conjecture formula';
    }

    return 1;
}

1;
__END__

=pod

=head1 NAME

tipi conjecture

=head1 SYNOPSIS

tipi conjecure [--man | --help]

tipi conjecture [--name-only] TPTP-file

=head1 DESCRIPTION

Given a TPTP problem file, B<tipi conjecture> extracts the conjecture,
if present.  It prints the whole TPTP formula representing the
conjecture.  If the C<--name-only> option is given, only the name of
the conjecture formula is printed.

=cut
