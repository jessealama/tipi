package Parser;

use base qw(Exporter);
use strict;
use warnings;
use Parse::RecDescent;
use Data::Dumper;
use charnames qw(:full);
use Regexp::DefaultFlags;
use Carp qw(carp);
use Readonly;

our @EXPORT_OK = qw(parse);

# $::RD_HINT = 1;

Readonly my $EMPTY_STRING => q{};

my $grammar = join ($EMPTY_STRING, <DATA>);

my $parser = Parse::RecDescent->new ($grammar);

sub delete_tptp_comments {
    my $text = shift;
    my @final_lines = ();
    my @lines = split ("\N{LF}", $text);
    foreach my $line (@lines) {
	$line =~ s/ [%].*\z / /;
	push (@final_lines, $line);
    }
    return join ("\N{LF}", @final_lines);
}

sub parse {
    my $text = shift;
    $text = delete_tptp_comments ($text);
    return $parser->tptp_file ($text);
}

1;
__DATA__

{use Data::Dumper;}

<autotree>

 # <autoaction: {
 #      foreach my $k (keys %item) {
 #      	if (defined $item{$k}) {
 #      	    print $k, ' : ', Dumper ($item{$k});
 #      	} else {
 #      	    print $k, ' : (undefined)', "\n";
 #      	}
 #      }
 #     [@item];
 # } >

tptp_file: tptp_input(s?) eofile

tptp_input: annotated_formula | include

annotated_formula: fof_annotated

fof_annotated:
    'fof' '(' name ',' formula_role ',' fof_formula ')' '.'
  | <error>

variable: upper_word

number: integer | rational | real

integer: signed_integer | unsigned_integer

signed_integer: sign unsigned_integer

unsigned_integer: decimal

decimal: zero_numeric | positive_decimal

zero_numeric: '0'

positive_decimal: non_zero_number numeric(s?)

non_zero_number: /[1-9]/

non_zero_numeric: /[1-9]/

rational: signed_rational | unsigned_rational

signed_rational: sign unsigned_rational

unsigned_rational: decimal slash positive_decimal

slash: '/'

positive_decimal: non_zero_numeric numeric(s?)

real: signed_real | unsigned_real

signed_real: sign unsigned_real

unsigned_real: decimal_fraction | decimal_exponent

decimal_fraction: decimal dot_decimal

dot_decimal: dot numeric numeric(s?)

dot: '.'

decimal_exponent: (decimal | decimal_fraction) exponent integer

exponent: /[Ee]/

sign: /[+-]/

distinct_object: double_quote do_char '*' double_quote

double_quote: /["]/

do_char: /[a-z]/

formula_data: '$' 'fof' '(' fof_formula ')'

atomic_word: lower_word | single_quoted

single_quoted: single_quote sq_char sq_char(s?) single_quote

single_quote: /[']/

sq_char: /[a-z]/

formula_role: lower_word

null: ''

lower_word: lower_alpha alpha_numeric(s?)

upper_word: upper_alpha alpha_numeric(s?)

lower_alpha: /[a-z]/

upper_alpha: /[A-Z]/

numeric: /[0-9]/

alpha_numeric: /[a-zA-Z0-9_]/

fof_formula:
    fof_logic_formula
  | fof_sequent

fof_logic_formula:
    fof_binary_formula
  | fof_unitary_formula

fof_binary_formula:
    fof_binary_nonassoc
  | fof_binary_assoc

fof_binary_nonassoc:
  fof_unitary_formula
  ('<=>' | '=>' | '<=' | '<~>' | '~|' | '~&')
  fof_unitary_formula

fof_binary_assoc:
    fof_or_formula
  | fof_and_formula

fof_or_formula:
    fof_unitary_formula '|' (fof_or_formula | fof_unitary_formula)

fof_and_formula:
    fof_unitary_formula '&' (fof_and_formula | fof_unitary_formula)

fof_unitary_formula:
    fof_quantified_formula
  | fof_unary_formula
  | atomic_formula
  | '(' fof_logic_formula ')'

fof_quantified_formula:
    fol_quantifier
    '[' fof_variable_list ']'
    ':'
    fof_unitary_formula

fof_variable_list:
    variable ',' <commit> fof_variable_list
  | variable

fof_unary_formula: unary_connective fof_unitary_formula | fol_infix_unary

fol_infix_unary: term infix_equality term

fol_quantifier: '!' | '?'

atomic_formula:
    plain_atomic_formula
  | defined_atomic_formula

plain_atomic_formula: plain_term

plain_term:
    functor '(' <commit> arguments ')'
  | constant
constant: functor
functor: atomic_word
arguments:
    term ',' arguments
  | term
term: functor_term | variable
functor_term: plain_term | defined_term
defined_term: defined_atom | defined_atomic_term
defined_atom: number | distinct_object
defined_atomic_term: defined_plain_term

defined_plain_term:
    defined_functor '(' <commit> arguments ')'
  | defined_constant

defined_constant: defined_functor
defined_functor: atomic_defined_word
atomic_defined_word: dollar_word
dollar_word: '$' lower_word
include: 'include' '(' file_name ')' '.'

atomic_system_word: dollar_dollar_word
dollar_dollar_word: '$$' lower_word

defined_atomic_formula: defined_plain_formula | defined_infix_formula

unary_connective: '~'

name: atomic_word | integer
infix_equality: '='
file_name: single_quoted
formula_selection: ',' '[' name_list ']' | null
name_list: name | name ',' name_list
defined_plain_formula: defined_plain_term
defined_plain_term: defined_constant | defined_functor '(' arguments ')'
defined_infix_formula: term defined_infix_pred term
defined_infix_pred: infix_equality
fof_sequent: fof_tuple gentzen_arrow fof_tuple | '(' fof_sequent ')'
fof_tuple: '[' ']' | '[' fof_tuple_list ']'
fof_tuple_list: fof_logic_formula | fof_logic_formula ',' fof_tuple_list
gentzen_arrow: '-->'

eofile: /\Z/

comment: comment_line
comment_line: /[%].*/
printable_char: /./
