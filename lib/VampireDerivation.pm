package VampireDerivation;

use Moose;
use Regexp::DefaultFlags;
use charnames qw(:full);
use English qw(-no_match_vars);

extends 'Derivation';

sub BUILD {
    my $self = shift;
    my $raw_text = $self->get_raw_text ();

    my %used_premises = ();
    my $theory = $self->get_background_theory ();
    my $conjecture = $theory->has_conjecture_formula () ? $theory->get_conjecture ()
	: undef;
    my $conjecture_name = defined $conjecture ? $conjecture->get_name () : undef;

    my @lines = split / \N{LF} /, $raw_text;

    foreach my $line (@lines) {
	if ($line =~ / [[] input \N{SPACE} (.+) []] \z/) {
	    my $used_formula_name = $1;
	    $used_premises{$used_formula_name} = 0;
	} elsif ($line =~ / [[] negated \N{SPACE} conjecture []]/ ) {
	    if (defined $conjecture_name) {
		$used_premises{$conjecture_name} = 0;
	    }
	}
    }

    my @used_formulas = keys %used_premises;


    # my @axioms = $theory->get_axioms ();
    # foreach my $formula_name (keys %used_premises) {
    # 	my $formula = $theory->formula_with_name ($formula_name);
    # 	push (@used_formulas, $formula);
    # }

    $self->_set_used_premises (\@used_formulas);

}

1;
__END__
