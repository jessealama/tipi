package Result;

use Moose;
use Carp qw(croak);
use charnames qw(:full);
use Regexp::DefaultFlags;
use Carp qw(croak carp);
use Readonly;
use Data::Dumper;

# Our modules
use EproverDerivation;
use VampireDerivation;
use Prover9Derivation;
use ParadoxInterpretation;
use SZS qw(known_szs_status);

Readonly my $LF => "\N{LF}";
Readonly my $SPACE => q{ };
Readonly my $SZS_UNKNOWN => 'Unknown';
Readonly my $SZS_ERROR => 'Error';

has 'tool' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_tool',
);

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
    is => 'ro',
    reader => 'get_exit_code',
);

has intended_szs_status => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_intended_szs_status',
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

    my $tool = $self->get_tool ();
    my $output = $self->get_output ();
    my $background = $self->get_background_theory ();

    if (! defined $output) {
	croak 'Error: the output slot of this Result object is undefined.';
    }

    if ($tool eq 'eprover') {
	return EproverDerivation->new (raw_text => $output,
				       background_theory => $background);
    } elsif ($tool eq 'vampire') {
	return VampireDerivation->new (raw_text => $output,
				       background_theory => $background);
    } elsif ($tool eq 'prover9') {
	return Prover9Derivation->new (raw_text => $output,
				       background_theory => $background);
    } else {
	croak 'Unable to interpret the output of', $SPACE, $tool, $SPACE, 'as a derivation';
    }


}

sub has_szs_status {
    my $self = shift;
    my $output = $self->get_output ();

    # carp 'Output:', "\N{LF}", $output;

    if ($output =~ / SZS \N{SPACE} status \N{SPACE} ([a-zA-Z]+) /m) {
	return 1;
    } else {
	return 0;
    }
}

Readonly my $PROVER9_PROOF_TOKEN
    => '============================== PROOF =================================';

sub get_szs_status {
    my $self = shift;

    my $tool = $self->get_tool ();
    my $output = $self->get_output ();

    if (! $self->exited_cleanly ()) {
	# carp 'Whoops, I did not exit cleanly:', $LF, Dumper ($self);
	return $SZS_ERROR;
    } elsif ($tool eq 'prover9') { # ugh
	my $intended_szs_status = $self->get_intended_szs_status ();
	if (index $output, $PROVER9_PROOF_TOKEN) {
	    return $intended_szs_status;
	} else {
	    return $SZS_UNKNOWN;
	}
    } elsif ($output =~ / SZS \N{SPACE} status \N{SPACE} ([a-zA-Z]+) /m) {
	my $status = $1;
	if (known_szs_status ($status)) {
	    return $status;
	} else {
	    carp 'Unknown SZS status \'', $status, '\'; defaulting to \'Unknown\'.';
	    return $SZS_UNKNOWN;
	}
    } else {
	return $SZS_UNKNOWN;
    }
}

sub output_as_model {
    my $self = shift;
    my $output = $self->get_output ();
    my $background = $self->get_background_theory ();

    if (! defined $output) {
	croak 'Error: the output slot of this Result object is undefined.';
    }

    return ParadoxInterpretation->new (raw_text => $output,
				       background_theory => $background);
}

1;
__END__
