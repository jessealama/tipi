#!perl -T

use Test::More qw(no_plan);
use Readonly;

Readonly my $SZS_THEOREM => 'Theorem';

use_ok ('SZS', 'known_szs_status');

ok (known_szs_status ($SZS_THEOREM));



# -*- mode:perl; -*-
