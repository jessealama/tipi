package Derivation;

use Moose;
use Data::Dumper;
use Carp qw(croak carp);
use File::Temp qw(tempfile);

has 'background_theory' => (
    isa => 'Theory',
    is => 'ro',
    reader => 'get_background_theory',
    required => 1,
);

has 'raw_text' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_raw_text',
);

has 'used_premises' => (
    is => 'rw',
    isa => 'ArrayRef',
    reader => '_get_used_premises',
    writer => '_set_used_premises',
);

sub get_used_premises {
    my $self = shift;
    my $used_premises_ref = $self->_get_used_premises ();
    my @used_premises = @{$used_premises_ref};
    if (wantarray) {
	return @used_premises;
    } else {
	return \@used_premises;
    }
}

sub get_unused_premises {
    my $self = shift;
    my $background = $self->get_background_theory ();

    my @axioms = $background->get_axioms ();
    my @used = $self->get_used_premises ();

    my @axiom_names = map { $_->get_name () } @axioms;
    my @used_names = map { $_->get_name () } @used;

    my %unused = ();
    foreach my $axiom (@axiom_names) {
	$unused{$axiom} = 0;
    }

    foreach my $formula (@used_names) {
	delete $unused{$formula};
    }

    return map { $background->formula_with_name ($_) } keys %unused;
}

sub theory_from_used_premises {
    my $self = shift;

    my $theory = $self->get_background_theory ();
    my @used_premises = @{$self->get_used_premises ()};
    my $conjecture = $theory->get_conjecture ();

    (my $tmp_theory_fh, my $tmp_theory_path) = tempfile ()
	or croak 'Failed to create a temporary file.';

    my @tptp_used_premises = map { $_->tptpify () } @used_premises;

    foreach my $tptp_formula (@tptp_used_premises) {
	print {$tmp_theory_fh} $tptp_formula, "\n";
    }

    close $tmp_theory_fh
	or croak 'Unable to close the output filehandle for our temporary theory file.';

    # carp 'Contents of the new temporary theory:', "\n", `cat $tmp_theory_path`;

    return Theory->new (path => $tmp_theory_path);
}

1;
__END__
