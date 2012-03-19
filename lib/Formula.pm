package Formula;

use Moose;
use Regexp::DefaultFlags;
use charnames qw(:full);
use Carp qw(croak);
use Readonly;

Readonly my $TWO_SPACES => q{  };

has 'name' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_name',
);

has 'status' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_status',
);

has 'formula' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_formula',
);

sub make_formula {
    my $formula_string = shift;

    if (grep { / \N{LF} / } $formula_string) {
	croak 'Unable to parse the TPTP formula string', "\n", "\n", $TWO_SPACES, $formula_string;
    } elsif ($formula_string =~ /\A fof \( ([^,]+), ([^,]+), (.+) \) [.] \z/) {
	(my $name, my $status, my $content) = ($1, $2, $3);
	return Formula->new (name => $name,
			     status => $status,
			     formula => $content);
    } else {
	croak 'Unable to parse the TPTP formula string', "\n", "\n", $TWO_SPACES, $formula_string;
    }
}

sub tptpify {
    my $self = shift;
    my $name = $self->get_name ();
    my $status = $self->get_status ();
    my $content = $self->get_formula ();
    return "fof(${name},${status},${content}).";
}

1;
__END__
