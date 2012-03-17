package TPTP;

use Moose;
use IPC::Cmd qw(can_run);
use IPC::Run qw(run start timer harness);
use Carp qw(croak);
use Readonly;

use Utils qw(ensure_readable_file);

Readonly my $TPTP4X => 'tptp4X';
Readonly my $EMPTY_STRING => q{};

has 'path' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_path',
    required => 1,
);

sub ensure_tptp4x_available {
    return can_run ($TPTP4X);
}

sub ensure_valid_tptp_file {
    my $path = shift;

    my @tptp4x_call = ($TPTP4X, '-N', '-V', '-c', '-x', $path);
    my $tptp4x_out = $EMPTY_STRING;
    my $tptp4x_err = $EMPTY_STRING;
    my $tptp4x_harness = harness (\@tptp4x_call,
				  '>', \$tptp4x_out,
				  '2>', \$tptp4x_err);

    $tptp4x_harness->start ();
    $tptp4x_harness->finish ();

    my $tptp4x_exit_code = $tptp4x_harness->result (0);
    return ($tptp4x_exit_code == 0);
}

sub BUILD {

    my $self = shift;

    my $path = $self->get_path ();

    if ( ! ensure_readable_file ($path)) {
	croak ('There is no file at ', $path, ', or it is unreadable.');
    }

    if (! ensure_tptp4x_available ()) {
	croak ('The needed tptp4X program seems to be unavailable.');
    }

    if (! ensure_valid_tptp_file ($path)) {
	croak ('The file at ', $path, ' is not a valid TPTP file.');
    }

    return $self;

}

1;
__END__
