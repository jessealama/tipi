package Result;

use Moose;
use Carp qw(croak);

use EproverDerivation;

has 'derivation' => (
    isa => 'Derivation',
    is => 'ro',
    reader => 'get_derivation',
);

has 'timed_out' => (
    isa => 'Bool',
    is => 'ro',
    reader => 'get_timed_out',
);

sub timed_out {
    my $self = shift;
    return $self->get_timed_out ();
}

has 'exit_code' => (
    isa => 'Int',
    is => 'ro',
    reader => 'get_exit_code',
);

has 'error_output' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_error_output',
);

has 'output' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_output',
);

has 'background_theory' => (
    isa => 'Theory',
    is => 'ro',
    reader => 'get_background_theory',
    required => 1,
);

sub exited_cleanly {
    my $self = shift;
    my $exit_code = $self->get_exit_code ();
    return (defined $exit_code && $exit_code == 0);
}

sub output_as_derivation {
    my $self = shift;

    my $output = $self->get_output ();
    my $background = $self->get_background_theory ();

    if (! defined $output) {
	croak 'Error: the output slot of this Result object is undefined.';
    }

    return EproverDerivation->new (raw_text => $output,
			           background_theory => $background);
}

1;
__END__
