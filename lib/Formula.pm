package Formula;

use Moose;
use Pod::Find qw(pod_where);
use Pod::Usage;
use Regexp::DefaultFlags;
use charnames qw(:full);
use Carp qw(croak carp);
use Readonly;
use Term::ANSIColor qw(colored);
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

use Utils qw(tptp_xmlize
	     apply_stylesheet);

# Strings
Readonly my $TWO_SPACES => q{  };
Readonly my $EMPTY_STRING => q{};

# Stylesheets
Readonly my $XSL_HOME => "$RealBin/../xsl";
Readonly my $TPTP_INFO_STYLESHEET => "${XSL_HOME}/tptp-info.xsl";

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

    (my $tmp_fh, my $tmp_path) = tempfile ();

    print {$tmp_fh} $formula_string
	or confess 'Error: unable to print \'', $formula_string, '\' to a temporary filehandle: ', $!;
    close $tmp_fh
	or confess 'Error: unable to close a temporary output filehandle: ', $!;

    tptp_xmlize ($tmp_path, $tmp_path);

    my $kind = apply_stylesheet ($TPTP_INFO_STYLESHEET,
				 $tmp_path,
				 undef,
				 {
				     'field' => 'syntax',
				 });

    my $name = apply_stylesheet ($TPTP_INFO_STYLESHEET,
				 $tmp_path,
				 undef,
				 {
				     'field' => 'name',
				 });

    # carp 'name = ', $name;

    my $status = apply_stylesheet ($TPTP_INFO_STYLESHEET,
				   $tmp_path,
				   undef,
				   {
				     'field' => 'status',
				 });

    my $content = apply_stylesheet ($TPTP_INFO_STYLESHEET,
				    $tmp_path,
				    undef,
				    {
					'field' => 'formula',
				    });

    # carp 'content = ', $content;

    if ($kind eq 'formula') {
	$kind = 'fof';
    } elsif ($kind eq 'clause') {
	$kind = 'cnf';
    } else {
	confess 'Error: unknown formula kind \'', $kind, '\'.';
    }

    return Formula->new (kind => $kind,
			 name => $name,
			 status => $status,
			 formula => $content);

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

=head1 NAME

=head1 DESCRIPTION

Formula is a package (specifically, a Moose class) that contains some
functionality for working with TPTP formulas.

=head1 DEPENDENCIES

=over 8

=item L<Moose|http://search.cpan.org/~doy/Moose-2.0403/lib/Moose.pm>

=back

=head1 SEE ALSO

=over 8

=item L<Getting started with TPTP and TSTP/http://www.cs.miami.edu/~tptp/TPTP/QuickGuide/>

One can see here how TPTP formulas are specified and used.

=back

=cut
