package Command;

use Moose;

has 'description' => (
    isa => 'Str',
    is => 'rw',
    reader => 'describe',
    writer => '_set_description',
);

# sub execute { my @arguments; ... }

1;
__END__
