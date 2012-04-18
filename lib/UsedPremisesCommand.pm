package UsedPremisesCommand;

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
	     slurp);
use SZS qw(is_szs_success
	   szs_implies
	   known_szs_status);

Readonly my $STDIN_PATH => q{--};
Readonly my $TWO_SPACES => q{  };
Readonly my $FULL_STOP => q{.};
Readonly my $SPACE => q{ };
Readonly my $LF => "\N{LF}";
Readonly my $EMPTY_STRING => q{};
Readonly my $DESCRIPTION => 'Extract the used premises from a derivation';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'yellow';

my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_background_theory_path = undef;

sub print_formula_names_with_color {
    my $formulas_ref = shift;
    my $color = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my @formulas = @{$formulas_ref};

    if (defined $parameters{'sorted'} && $parameters{'sorted'}) {
	my @formulas_sorted = sort @formulas;
	my @formulas_colored
	    = map { colored ($_, $color) } @formulas_sorted;
	say join ("\N{LF}", @formulas_colored);
    } else {
	my @formula_names_colored = map { color ($_, $color) } @formulas;
	say join ("\N{LF}", @formula_names_colored);
    }

    return 1;

}

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
	'debug' => \$opt_debug,
	'help|?' => \$opt_help,
	'background-theory=s' => \$opt_background_theory_path,
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
	pod2usage (-msg => error_message ('Please supply a derivation file.'),
		   -exitval => 2);
    }

    if (scalar @arguments > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
		   -exitval => 2);
    }

    if (! ensure_tptp4x_available ()) {
	say STDERR error_message ('Cannot run tptp4X');
	exit 1;
    }

    if (defined $opt_background_theory_path) {

	if ($opt_background_theory_path eq $EMPTY_STRING) {
	    say {*STDERR} error_message ('The empty string is not an appropriate name of a background theory.');
	    exit 1;
	}

	if (! ensure_readable_file ($opt_background_theory_path)) {
	    say {*STDERR} error_message ('The file at', $SPACE, $opt_background_theory_path, $SPACE, 'does not exist (or is unreadable).');
	    exit 1;
	}

	if (! ensure_sensible_tptp_theory ($opt_background_theory_path)) {
	    my $errors = tptp4X_output ($opt_background_theory_path);
	    say {*STDERR} error_message ('The file at ', $opt_background_theory_path, ' is not a valid TPTP file.');
	    say {*STDERR} 'Here is what tptp4X said:';
	    say {*STDERR} $errors;
	    exit 1;
	}

    }

    my $derivation_file = $arguments[0];

    if (! ensure_readable_file ($derivation_file)) {
	say {*STDERR} error_message ('The file at', $SPACE, $derivation_file, $SPACE, 'does not exist (or is unreadable).');
	exit 1;
    }

    return $self->$orig (@arguments);

};

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $derivation_file = $arguments[0];

    my $derivation_contents = slurp ($derivation_file);

    my $derivation = undef;
    my $background_theory
	= defined $opt_background_theory_path
	    ? Theory->new (path => $opt_background_theory_path)
		: undef;

    # Sniff around
    if ($derivation_contents =~ /Hi \N{SPACE} Geoff/) {
	$derivation
	    = VampireDerivation->new (raw_text => $derivation_contents,
				      background_theory => $background_theory);
    } elsif ($derivation_contents =~ / \A \N{SPACE}+ \d+ \N{SPACE} [:] /) {
	$derivation
	    = EproverDerivation->new (raw_text => $derivation_contents,
				      background_theory => $background_theory);
    } elsif ($derivation_contents =~ / === \N{SPACE} PROOF \N{SPACE} === /) {
	$derivation
	    = Prover9Derivation->new (raw_text => $derivation_contents,
				      background_theory => $background_theory);
    } else {
	confess 'Unable to determine what kind of derivation', $SPACE, $derivation_file, $SPACE, 'is.', $LF, '(We failed to make sense of the file as a Vampire, Eprover, or Prover9 derivation.)';
    }

    my @used_premises = $derivation->get_used_premises ();
    my @unused_premises
	= defined $opt_background_theory_path
	    ? $derivation->get_unused_premises ()
		: ();

    # Sanity check: all the used premises are present in the
    # background theory

    if (defined $opt_background_theory_path) {
	foreach my $premise (@used_premises) {
	    if (! $background_theory->is_known_formula_name ($premise)) {
		confess 'The premise', $SPACE, $premise, $SPACE, 'appears in the derivation at', $LF, $LF, $TWO_SPACES, $derivation_file, $LF, $LF, 'but there is no formula with that name in the background theory', $LF, $LF, $TWO_SPACES, $opt_background_theory_path, $LF;
	    }
	}

	say 'PREMISES', $SPACE, '(', colored ('used', $USED_PREMISE_COLOR), $SPACE, '/', $SPACE, colored ('unused', $UNUSED_PREMISE_COLOR), ')';

	if (scalar @used_premises > 0) {
	    print_formula_names_with_color (\@used_premises,
					    $USED_PREMISE_COLOR,
					    { 'sorted' => 1,
					      'theory' => $background_theory });
	}
	if (scalar @unused_premises > 0) {
	    print_formula_names_with_color (\@unused_premises,
					    $UNUSED_PREMISE_COLOR,
					    { 'sorted' => 1,
					      'theory' => $background_theory });
	}

    } else {
	print_formula_names_with_color (\@used_premises,
					undef,
					{ 'sorted' => 1 });

    }

    return 1;

}

1;
__END__

=pod

=cut
