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

=cut
