use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Tipi',
    license             => 'perl',
    dist_author         => 'Jesse Alama <jesse.alama@gmail.com>',
    dist_version_from   => 'lib/Tipi.pm',
    requires => {
        'Test::More' => 0,
        'version'    => 0,
    },
    add_to_cleanup      => [ 'Tipi-*' ],
);

$builder->create_build_script();
