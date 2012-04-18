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

=cut
