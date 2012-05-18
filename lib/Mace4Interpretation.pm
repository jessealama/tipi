package Mace4Interpretation;

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
	if ($line =~ /\A [%] Interpretation \N{SPACE} of \N{SPACE} size \N{SPACE} \d+ \z/ ) {
	    $within_model = 0;
	}
	if ($within_model) {
	    push (@model_lines, $line);
	}
    }

    my $description = join ("\N{LF}", @model_lines);

    return "${description}\N{LF}";
}

1;
__END__

=pod

=head1 NAME

Mace4Interpretation

=head1 DESCRIPTION

This is a subclass of the Interpretation class devoted to extracting
information from the output of the Mace4 model finding program.

=head1 DEPENDENCIES

=over 8

=item L<Moose|http://search.cpan.org/~doy/Moose-2.0403/lib/Moose.pm>

=back

=head1 SEE ALSO

=over 8

=item L<Prover9 and Mace4|http://www.cs.unm.edu/~mccune/prover9/>

=back

=cut
