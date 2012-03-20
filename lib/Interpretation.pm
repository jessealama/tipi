package Interpretation;

use Moose;

has 'raw_text' => (
    is => 'ro',
    isa => 'Str',
    reader => 'get_raw_text',
);

has 'background_theory' => (
    is => 'ro',
    isa => 'Theory',
    reader => 'get_background_theory',
);

# sub domain { return }

1;
__END__
