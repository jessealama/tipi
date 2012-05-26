package EproverDerivation;

use Moose;
use Pod::Find qw(pod_where);
use Pod::Usage;
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
	if ($line =~ / initial \( "[^"]+", \N{SPACE} (.+) \) /) {
	    my $used_formula_name = $1;
	    $used_premises{$used_formula_name} = 0;
	}
    }

    my @used_formulas = keys %used_premises;

    # my $theory = $self->get_background_theory ();
    # my @axioms = $theory->get_axioms (1);

    # foreach my $formula_name (keys %used_premises) {
    # 	my $formula = $theory->formula_with_name ($formula_name);
    # 	push (@used_formulas, $formula);
    # }

    $self->_set_used_premises (\@used_formulas);

}

1;
__END__

=pod

=head1 NAME

EproverDerivation

=head1 DESCRIPTION

EproverDerivation is a subclass of Derivation that extracts used
premises from proofs output by the E theorem prover.

=head1 SEE ALSO

=over 8

=item L<The E theorem prover|http://www.eprover.org/>

=back

=cut
