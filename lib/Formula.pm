package Formula;

use Moose;
use Regexp::DefaultFlags;
use charnames qw(:full);
use Carp qw(croak);
use Readonly;
use Term::ANSIColor qw(colored);

Readonly my $TWO_SPACES => q{  };
Readonly my $EMPTY_STRING => q{};

has 'kind' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_kind',
);

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
    } elsif ($formula_string =~ /\A ([a-z]+) [(] ([^,]+), ([^,]+), (.+) [)] [.] \z/) {
	(my $kind, my $name, my $status, my $content) = ($1, $2, $3, $4);
	return Formula->new (kind => $kind,
			     name => $name,
			     status => $status,
			     formula => $content);
    } else {
	croak 'Unable to parse the TPTP formula string', "\n", "\n", $TWO_SPACES, $formula_string;
    }
}

sub tptpify {
    my $self = shift;
    my $kind = $self->get_kind ();
    my $name = $self->get_name ();
    my $status = $self->get_status ();
    my $content = $self->get_formula ();
    return "${kind}(${name},${status},${content}).";
}

sub change_status {
    my $self = shift;
    my $new_status = shift;

    if (! defined $new_status) {
	croak 'To change the status of a formula, one must supply a new status.';
    }

    if ($new_status eq $EMPTY_STRING) {
	croak 'The empty string is not an acceptable formula status.';
    }

    if ($new_status =~ / \N{LF} /m) {
	croak 'A TPTP formula status cannot contain a linefeed character.';
    }

    if ($new_status =~ / \s /) {
	croak 'A TPTP formula status cannot contain whitespace.';
    }

    if ($new_status !~ / [a-z_]+ /) {
	croak 'A TPTP formula status must consist of lowercase alphabetic characters and the underscore character \'_\'.';
    }

    my $formula = $self->get_formula ();
    my $name = $self->get_name ();
    my $kind = $self->get_kind ();

    return Formula->new (kind => $kind,
			 status => $new_status,
			 name => $name,
			 formula => $formula);
}

sub make_axiom {
    my $self = shift;
    return $self->change_status ('axiom');
}

sub make_conjecture {
    my $self = shift;
    return $self->change_status ('conjecture');
}

sub negate {
    my $self = shift;

    my $formula = $self->get_formula ();

    return Formula->new (
	kind => $self->get_kind (),
	status => $self->get_status (),
	name => $self->get_name (),
	formula => "~ ( ${formula} )",
    );

}

sub name_with_color {
    my $self = shift;
    my $color = shift;

    my $name = $self->get_name ();
    return colored ($name, $color);
}

sub equal_to {
    my $self = shift;
    my $other_formula = shift;

    my $self_kind = $self->get_kind ();
    my $self_name = $self->get_name ();
    my $self_status = $self->get_status ();
    my $self_formula = $self->get_formula ();

    my $other_formula_kind = $other_formula->get_kind ();
    my $other_formula_name = $other_formula->get_name ();
    my $other_formula_status = $other_formula->get_status ();
    my $other_formula_formula = $other_formula->get_formula ();

    return ($self_kind eq $other_formula_kind
		&& $self_name eq $other_formula_name
		    && $self_status eq $other_formula_status
			&& $self_formula eq $other_formula_formula);
}

sub is_first_order {
    my $self = shift;
    my $kind = $self->get_kind ();

    return $kind ne 'tff';
}

1;
__END__

=pod

=cut
