package EproverDerivation;

use Moose;
use Regexp::DefaultFlags;
use charnames qw(:full);
use English qw(-no_match_vars);

extends 'Derivation';

sub BUILD {
    my $self = shift;
    my $raw_text = $self->get_raw_text ();

    my %used_premises = ();

    my @lines = split / \N{LF} /, $raw_text;

    foreach my $line (@lines) {
	if ($line =~ / initial \( "[^"]+", \N{SPACE} (.+) \) \z/) {
	    my $used_formula_name = $1;
	    $used_premises{$used_formula_name} = 0;
	}
    }

    my $theory = $self->get_background_theory ();
    my @axioms = $theory->get_axioms (1);

    my @used_formulas = ();
    foreach my $formula_name (keys %used_premises) {
	my $formula = $theory->formula_with_name ($formula_name);
	push (@used_formulas, $formula);
    }

    $self->_set_used_premises (\@used_formulas);

}

1;
__END__
