package Proof;

use Moose;
use IPC::Cmd qw(can_run);
use IPC::Run qw(run start timer harness);
use Carp qw(croak confess carp);
use Readonly;
use charnames qw(:full);
use List::MoreUtils qw(firstidx any none);
use File::Temp qw(tempfile);
use Regexp::DefaultFlags;
use Data::Dumper;

# Our modules
use Utils qw(ensure_readable_file
	     slurp);
use Formula;

extends 'Theory';

1;
__END__

=pod

=head1 NAME

Theory

=head1 DESCRIPTION

This is a Moose class that represents TSTP proofs.

=head1 DEPENDENCIES

=over 8

=item L<Moose|http://search.cpan.org/~doy/Moose-2.0403/lib/Moose.pm>

=back

=cut
