package Command;

use Pod::Find qw(pod_where);
use Pod::Usage;
use Moose;

has 'description' => (
    isa => 'Str',
    is => 'rw',
    reader => 'describe',
    writer => '_set_description',
);

# sub execute { my @arguments; ... }

1;
__END__

=pod

=head1 NAME

Command

=head1 DESCRIPTION

This is the base class of all Tipi commands.  It does not do anything.

=head1 DEPENDENCIES

=over 8

=item L<Moose|http://search.cpan.org/~doy/Moose-2.0403/lib/Moose.pm>

=back

=cut
