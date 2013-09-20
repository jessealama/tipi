# -*- mode: perl; -*-

use strict;
use warnings;
use charnames qw(:full);
use File::Spec::Functions qw(catfile catdir updir rel2abs);
use FindBin qw($RealBin);
use Readonly;
use Test::More qw(no_plan);

my @needed_Formula_symbols = qw(parse_tptp_formula parse_fof_formula parse_tptp_file);

foreach my $symbol (@needed_Formula_symbols) {
    use_ok ('Formula', $symbol)
        or BAIL_OUT ("Cannot load symbol '${symbol}' from the Formula module");
}
use_ok ('Data::Dumper');
use_ok ('Utils', qw(files_in_directory))
    or BAIL_OUT ('Cannot load Utils module');

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

# try parsing TPTP files
ok (-d $RealBin,
    '$RealBin is a directory')
    or BAIL_OUT ('$RealBin is not a directory');
my $data_dir = catdir ($RealBin, 'data');
ok (-d $data_dir,
    'data directory exists')
    or BAIL_OUT ('data directory does not exist');
my @problems = files_in_directory ($data_dir);
foreach my $problem (@problems) {
    my $parsed_problem = parse_tptp_file ($problem);
    ok (defined $parsed_problem,
        "${problem} can be parsed");
}
