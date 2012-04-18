package Prover9Derivation;

use Moose;
use Regexp::DefaultFlags;
use charnames qw(:full);
use English qw(-no_match_vars);
use Readonly;
use Carp qw(carp croak confess);
use Data::Dumper;

extends 'Derivation';

Readonly my $SPACE => q{ };
Readonly my $LF => "\N{LF}";

sub BUILD {
    my $self = shift;

    my $raw_text = $self->get_raw_text ();
    my $theory = $self->get_background_theory ();
    my @axioms = $theory->get_axioms (1);

    my %used_premises = ();

    my @assumption_lines = ();
    my @lines = split / \N{LF} /, $raw_text;

    foreach my $line (@lines) {
	if ($line =~ / [[] assumption []] [.] \z/) {
	    push (@assumption_lines, $line);
	}
    }

    # carp 'Assumption lines: ', $LF, Dumper (@assumption_lines);

    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	my $axiom_regexp = quotemeta $axiom_name;

	# carp 'Our regexp: ', $axiom_regexp;

	foreach my $assumption_line (@assumption_lines) {
	    if ($assumption_line =~ /[#] \N{SPACE} label [(] ${axiom_regexp} [)] /) {
		$used_premises{$axiom_name} = 0;
		last;
	    }
	}
    }

    # my @used_formulas = map { $theory->formula_with_name ($_) } keys %used_premises;

    my @used_formulas = keys %used_premises;

    $self->_set_used_premises (\@used_formulas);

    return $self;

}

1;
__END__

=pod

=head1 NAME

Prover9Derivation

=head1 DESCRIPTION

Prover9Derivation is a subclass of Derivation that extracts used
premises from proofs output by the Prover9 theorem prover.

=head1 SEE ALSO

=over 8

=item L<Prover9|http://www.cs.unm.edu/~mccune/prover9/>

=back

=cut
