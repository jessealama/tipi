package Derivation;

use Moose;
use Data::Dumper;
use Carp qw(croak carp);
use File::Temp qw(tempfile);
use Readonly;
use Regexp::DefaultFlags;
use charnames qw(:full);
use List::MoreUtils qw(none);

use feature 'say';

Readonly my $LF => "\N{LF}";

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

    my @axioms = $background->get_axioms (1);
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

    my @unused = map { $background->formula_with_name ($_) } keys %unused;

    if (wantarray) {
	return @unused;
    } else {
	return \@unused;
    }
}

sub theory_from_used_premises {
    my $self = shift;

    my $theory = $self->get_background_theory ();
    my @used_premises = @{$self->get_used_premises ()};
    my @used_premise_names = map { $_->get_name () } @used_premises;

    (my $tmp_theory_fh, my $tmp_theory_path) = tempfile ()
	or croak 'Failed to create a temporary file.';

    foreach my $formula (@used_premises) {
	print {$tmp_theory_fh} $formula->tptpify (), "\n";
    }

    if ($theory->has_conjecture_formula ()) {
	my $conjecture = $theory->get_conjecture ();
	my $conjecture_name = $conjecture->get_name ();
	if (none { $_->get_name () eq $conjecture_name } @used_premises) {
	    say {$tmp_theory_fh} $conjecture->tptpify ();
	}
    }

    close $tmp_theory_fh
	or croak 'Unable to close the output filehandle for our temporary theory file.';

    return Theory->new (path => $tmp_theory_path);
}

1;
__END__
