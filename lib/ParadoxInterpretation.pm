package ParadoxInterpretation;

use Moose;
use charnames qw(:full);
use Regexp::DefaultFlags;

extends 'Interpretation';

sub describe {
    my $self = shift;
    my $raw_output = $self->get_raw_text ();

    my @lines = split ("\N{LF}", $raw_output);
    my @model_lines = ();
    my $within_model = 0;
    foreach my $line (@lines) {
	if ($line =~ /SZS \N{SPACE} output \N{SPACE} end \N{SPACE} FiniteModel/ ) {
	    $within_model = 0;
	}
	if ($within_model) {
	    push (@model_lines, $line);
	}
	if ($line =~ /SZS \N{SPACE} output \N{SPACE} start \N{SPACE} FiniteModel/) {
	    $within_model = 1;
	}
    }

    my $description = join ("\N{LF}", @model_lines);

    return $description;
}

1;
__END__

=pod

=head1 NAME

ParadoxInterpretation

=head1 DESCRIPTION

This is a subclass of the Interpretation class devoted to extracting
information from the output of the Paradox *model finding program.

=head1 DEPENDENCIES

=over 8

=item L<Moose|http://search.cpan.org/~doy/Moose-2.0403/lib/Moose.pm>

=back

=head1 SEE ALSO

=over 8

=item L<New techniques that improve MACE-style finite model finding|http://www.cs.miami.edu/~geoff/Conferences/CADE/Archive/CADE-19/WS4/04.pdf>

=back

=cut
