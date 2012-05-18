package Interpretation;

use Moose;

has 'raw_text' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_raw_text',
);

has 'background_theory' => (
    is => 'ro',
    isa => 'Theory',
    reader => 'get_background_theory',
);

# sub domain { return }

1;
__END__

=pod

=head1 NAME

Interpretation

=head1 DESCRIPTION

This is the Moose base class for interpretations (first-order
structures).

=head1 DEPENDENCIES

=over 8

=item L<Moose|http://search.cpan.org/~doy/Moose-2.0403/lib/Moose.pm>

=back

=cut
