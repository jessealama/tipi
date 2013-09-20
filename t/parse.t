# -*- mode: perl; -*-

use strict;
use warnings;
use charnames qw(:full);
use Test::More tests => 13;

use_ok ('Formula', qw(parse_tptp_formula parse_fof_formula));
use_ok ('Data::Dumper');
use Readonly;

Readonly my $LF => "\N{LF}";

my $formula_1 = "include('hyperbolic.p').";
my $formula_2 = "include('hyperbolic.p',[a4,a5]).";
my $formula_3 = "include('../hyperbolic.p',${LF}[a5,${LF}a6,${LF}7,${LF}30]).";
my $formula_test = "p => q";
my $formula_4 = "fof(48,conjecture,${LF}    (! [A,B,C,D,E] :${LF}     ((between(A,B,C) & between(A,D,E))${LF}          => (? [F] : (between(B,F,E) & between(C,F,D)))))).";

my $parsed_1 = parse_tptp_formula ($formula_1);
my $parsed_2 = parse_tptp_formula ($formula_2);
my $parsed_3 = parse_tptp_formula ($formula_3);
my $parsed_4 = parse_tptp_formula ($formula_4);
my $parsed_test = parse_fof_formula (\$formula_test);

ok (defined $parsed_1, 'parse is defined');
ok ($parsed_1, 'parsed is not false');
ok (defined $parsed_2, 'parse is defined');
ok ($parsed_2, 'parsed is not false');
ok (defined $parsed_3, 'parse is defined');
ok ($parsed_3, 'parsed is not false');
ok (defined $parsed_4, 'parse is defined');
ok ($parsed_4, 'parsed is not false');
ok (defined $parsed_test, 'parse is defined');
ok ($parsed_test, 'parsed is not false');
is ($formula_test, '', 'consumed everything');

#warn Dumper ($parsed_test);
