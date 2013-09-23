# -*- mode: perl; -*-

use strict;
use warnings;
use charnames qw(:full);
use File::Spec::Functions qw(catfile catdir updir rel2abs);
use FindBin qw($RealBin);
use Readonly;
use File::Basename qw(basename);
use Test::More qw(no_plan);

my @needed_Formula_symbols = qw(parse_tptp_formula parse_fof_formula parse_tptp_file is_implication);

foreach my $symbol (@needed_Formula_symbols) {
    use_ok ('Formula', $symbol)
        or BAIL_OUT ("Cannot load symbol '${symbol}' from the Formula module");
}

use_ok ('Data::Dumper');

my @needed_Utils_symbols = qw(files_in_directory);
foreach my $symbol (@needed_Utils_symbols) {
    use_ok ('Utils', $symbol)
        or BAIL_OUT ("Cannot load symbol '${symbol}' from the Utils module");
}

Readonly my $LF => "\N{LF}";

my $formula_0 = '( f(X,X,Y) = X )';
my $formula_1 = "include('hyperbolic.p').";
my $formula_2 = "include('hyperbolic.p',[a4,a5]).";
my $formula_3 = "include('../hyperbolic.p',${LF}[a5,${LF}a6,${LF}7,${LF}30]).";
my $formula_test = "p => q";
my $formula_4 = "fof(48,conjecture,${LF}    (! [A,B,C,D,E] :${LF}     ((between(A,B,C) & between(A,D,E))${LF}          => (? [F] : (between(B,F,E) & between(C,F,D)))))).";

my $parsed_0 = parse_fof_formula ($formula_0);
ok (defined $parsed_0, "can parse '${formula_0}'");
my $parsed_1 = parse_tptp_formula ($formula_1);
my $parsed_2 = parse_tptp_formula ($formula_2);
my $parsed_3 = parse_tptp_formula ($formula_3);
my $parsed_4 = parse_tptp_formula ($formula_4);
my $parsed_test = parse_fof_formula ($formula_test);

ok (defined $parsed_1, "can parse ${formula_1}");
ok (defined $parsed_2, 'parse is defined');
ok (defined $parsed_3, 'parse is defined');
ok (defined $parsed_4, 'parse is defined');
ok (defined $parsed_test, 'parse is defined');

# need to ensure that the parses are as we expect, for example:
TODO: {
    local $TODO = 'Need to generate distinct kinds of formulas.';
    ok (is_implication ($parsed_test));
};

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

# big: try parsing axioms and problem files for the entire TPTP
my $tptp_dir = $ENV{'TPTP'};
ok (defined $tptp_dir, 'TPTP environment variable is set')
    or BAIL_OUT ('TPTP environment variable is not set.');
ok (-d $tptp_dir,
    'TPTP environment variable points to a directory')
    or BAIL_OUT ("The value of the TPTP environment variable, '${tptp_dir}', is not a directory.");
my $axioms_dir = catdir ($tptp_dir, 'Axioms');
ok (-d $axioms_dir,
    'TPTP Axioms directory exists')
    or BAIL_OUT ("The Axioms subdirectory of the TPTP directory does not exist at the expected location (${axioms_dir}).");
my @axiom_files = files_in_directory ($axioms_dir);
foreach my $file (@axiom_files) {
    my $base = basename ($file);
    my $result = parse_tptp_file ($file);
    ok (defined $result,
        "can parse ${base} axiom file");
}
