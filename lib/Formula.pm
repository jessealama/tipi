package Formula;

use Moose;
use Pod::Find qw(pod_where);
use Pod::Usage;
use Regexp::DefaultFlags;
use charnames qw(:full);
use Carp qw(croak carp);
use Readonly;
use Term::ANSIColor qw(colored);
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use Parse::RecDescent;
use base qw(Exporter);
use Utils qw(tptp_xmlize
	     apply_stylesheet);

our @EXPORT_OK = qw(parse_tptp_formula parse_fof_formula);

# Strings
Readonly my $TWO_SPACES => q{  };
Readonly my $EMPTY_STRING => q{};
Readonly my $LF => "\N{LF}";

# Stylesheets
Readonly my $XSL_HOME => "$RealBin/../xsl";
Readonly my $TPTP_INFO_STYLESHEET => "${XSL_HOME}/tptp-info.xsl";

has 'kind' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_kind',
);

has 'name' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_name',
);

has 'status' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_status',
);

has 'formula' => (
    isa => 'Str',
    is => 'ro',
    reader => 'get_formula',
);

sub BUILD {
    my $self = shift;
    # Ensure that everything is defined
    my $kind = $self->get_kind ();
    my $name = $self->get_name ();
    my $status = $self->get_status ();
    my $formula = $self->get_formula ();

    if (! defined $kind) {
        confess 'Cannot make a Formula without a kind.';
    }

    if (! defined $name) {
        confess 'Cannot make a Formula without a name.';
    }

    if (! defined $status) {
        confess 'Cannot make a Formula without a status.';
    }

    if (! defined $formula) {
        confess 'Cannot make a Formula without a formula.';
    }

    return $self;
}

Readonly my $TPTP_GRAMMAR_AUTOTREE =>
    q {<autotree>
          tptp_file: tptp_input(s?)
          tptp_input: annotated_formula | include
          annotated_formula: fof_annotated
          fof_annotated: 'fof' '(' name ',' formula_role ',' fof_formula annotations ')' '.'
          fof_annotated: 'fof' '(' name ',' formula_role ',' fof_formula ')' '.'
          annotations: ',' source optional_info
          source: dag_source | internal_source | external_source | 'unknown'
          source: '[' sources ']'
          sources: source ',' sources
          sources: source
          dag_source: name | inference_record
          inference_record: 'inference' '(' inference_rule ',' useful_info ',' inference_parents  ')'
          inference_parents: '[' parent_list ']'
          parent_list: parent_info ',' parent_list
          parent_list: parent_info
          parent_info: source parent_details
          parent_details: ':' general_list
          parent_details: ''
          inference_parents: '[' ']'
          inference_rule: atomic_word
          useful_info: general_list
          general_list: '[' ']'
          general_list: '[' general_terms ']'
          general_terms: general_term ',' general_terms
          general_terms: general_term
          general_term: general_data
          general_term: general_data ':' general_term
          general_term: general_list
          general_data: atomic_word | general_function | variable | number | distinct_object | formula_data
          general_function: atomic_word '(' general_terms ')'
          optional_info: ',' useful_info
          optional_info: ''
          internal_source: 'introduced' '(' intro_type optional_info ')'
          intro_type: 'definition' | 'axiom_of_choice' | 'tautology' | 'assumption'
          external_source: file_source | theory | creator_source
          file_source: 'file' '(' file_name file_info ')'
          file_info: ',' name
          file_info: ''
          theory: 'theory' '(' theory_name optional_info ')'
          theory_name: 'equality' | 'ac'
          creator_source: 'creator' '(' creator_name optional_info ')'
          creator_name: atomic_word
          name: atomic_word
          name: integer
          atomic_word: lower_word
          lower_word: /[a-z][a-zA-Z0-9_]*/
          lower_alpha: /[a-z]/
          upper_alpha: /[A-Z]/
          numeric: /[0-9]/
          alpha_numeric: /[a-z]/
          alpha_numeric: /[A-Z]/
          alpha_numeric: /[0-9]/
          alpha_numeric: '_'
          integer: signed_integer | unsigned_integer
          signed_integer: sign unsigned_integer
          sign: /[+-]/
          unsigned_integer: decimal
          decimal: zero_numeric | positive_decimal
          zero_numeric: '0'
          rational: signed_rational | unsigned_rational
          signed_rational: sign unsigned_rational
          unsigned_rational: decimal slash positive_decimal
          slash: '/'
          positive_decimal: non_zero_numeric numeric(s?)
          non_zero_numeric: /[1-9]/
          real: signed_real | unsigned_real
          signed_real: sign unsigned_real
          unsigned_real: '(' decimal_fraction ')'
          unsigned_real: '(' decimal_exponent ')'
          decimal_fraction: decimal dot_decimal
          dot_decimal: dot numeric(s)
          dot: '.'
          decimal_exponent: '(' decimal ')' exponent integer
          decimal_exponent: '(' decimal_fraction ')' exponent integer
          exponent: /[Ee]/
          positive_decimal: /[1-9]/ numeric(s?)
          formula_role: 'axiom' | 'hypothesis' | 'definition' | 'assumption' | 'lemma' | 'theorem' | 'conjecture' | 'negated_conjecture' | 'plain' | 'fi_domain' | 'fi_functors' | 'fi_predicates' | 'type' | 'unknown'
          fof_formula: fof_logic_formula # | fof_sequent
          fof_sequent: fof_tuple gentzen_arrow fof_tuple
          fof_sequent: '(' fof_sequent ')'
          fof_tuple: '[' fof_tuple_list ']'
          fof_tuple: '[' ']'
          fof_tuple_list: fof_logic_formula ',' fof_tuple_list
          fof_tuple_list: fof_logic_formula
          gentzen_arrow: '-->'
          fof_logic_formula: fof_binary_formula | fof_unitary_formula
          fof_binary_formula: fof_binary_nonassoc | fof_binary_assoc
          fof_binary_assoc: fof_or_formula | fof_and_formula
          fof_or_formula: fof_unitary_formula '|' fof_unitary_formula
          fof_or_formula: fof_unitary_formula '|' fof_or_formula
          fof_and_formula: fof_unitary_formula '&' fof_unitary_formula
          fof_and_formula: fof_unitary_formula '&' fof_and_formula
          fof_binary_nonassoc: fof_equivalence | fof_implication | fof_reverse_implication | fof_disequivalence | fof_nor | fof_nand
          fof_equivalence: fof_unitary_formula equivalence_connective fof_unitary_formula
          fof_implication: fof_unitary_formula implication_connective fof_unitary_formula
          fof_reverse_implication: fof_unitary_formula reverse_implication_connective fof_unitary_formula
          fof_disequivalence: fof_unitary_formula disequivalence_connective fof_unitary_formula
          fof_nor: fof_unitary_formula nor_connective fof_unitary_formula
          fof_nand: fof_unitary_formula nand_connective fof_unitary_formula
          equivalence_connective: '<=>'
          disequivalence_connective: '<~>'
          implication_connective: '=>'
          reverse_implication_connective: '<='
          nor_connective: '~|'
          nand_connective: '~&'
          fof_unitary_formula: '(' fof_logic_formula ')'
          fof_unitary_formula: atomic_formula | fof_quantified_formula | fof_unary_formula
          fof_quantified_formula: fol_quantifer '[' fof_variable_list ']' ':' fof_unitary_formula
          fol_quantifer: '!' | '?'
          fof_variable_list: variable ',' fof_variable_list | variable
          variable: upper_word
          upper_word: /[A-Z][a-zA-Z0-9_]*/
          single_quoted: "'" sq_char(s) "'"
          sq_char: /[a-z]/
          sq_char: /[A-Z]/
          sq_char: /[0-9]/
          sq_char: '/'
          sq_char: '.'
          sq_char: '\''
          do_char: /[a-zA-Z0-9]/
          do_char: /[\]["]/
          fof_unary_formula: unary_connective fof_unitary_formula | fol_infix_unary
          unary_connective: '~'
          fol_infix_unary: term infix_inequality term
          infix_inequality: '!='
          term: variable | function_term
          function_term: plain_term # | defined_term | system_term
          plain_term: functor '(' arguments ')'
          plain_term: constant
          functor: atomic_word
          constant: functor
          arguments: term ',' arguments
          arguments: term
          defined_term: defined_atom | defined_atomic_term
          defined_atom: number
          number: integer # | rational | real
          distinct_object: double_quote do_char(s?) double_quote
          double_quote: /["]/
          formula_data: '$fof' '(' fof_formula ')'
          defined_atomic_term: defined_plain_term
          defined_plain_term: defined_constant
          defined_plain_term: defined_functor '(' arguments ')'
          defined_constant: defined_functor
          defined_functor: '$uminus' | '$sum' | '$difference' | '$product' | '$quotient' | '$quotient_e' | '$quotient_t' | '$quotient_f' | '$remainder_e' | '$remainder_t' | '$remainder_f' | '$floor' | '$ceiling' | '$truncate' | '$round' | '$to_int' | '$to_rat' | '$to_real'
          system_term: system_constant
          system_term: system_functor '(' arguments ')'
          system_constant: system_functor
          system_functor: atomic_system_word
          atomic_system_word: dollar_dollar_word
          dollar_dollar_word: '$$' lower_word
          atomic_formula: plain_atomic_formula # | defined_atomic_formula | system_atomic_formula
          plain_atomic_formula: plain_term
          defined_atomic_formula: defined_plain_formula | defined_infix_formula
          defined_plain_formula: defined_plain_term
          defined_infix_formula: term defined_infix_pred term
          defined_infix_pred: infix_equality
          infix_equality: '='
          system_atomic_formula: system_term
          include: 'include' '(' file_name formula_selection ')' '.'
          include: 'include' '(' file_name ')' '.'
          file_name: single_quoted
          formula_selection: ',' '[' name_list ']'
          name_list: name ',' name_list
          name_list: name
  };

#$::RD_TRACE = 1;
#$::RD_HINT = 1;

# my $skip_prolog_comment = qr/[ \t\n]*/;
# $Parser::RecDescent::skip = $skip_prolog_comment;

my $parser = new Parse::RecDescent ($TPTP_GRAMMAR_AUTOTREE)
    or confess 'Bad grammar!';

sub parse_tptp_formula {
    my $text = shift;
    warn 'trying to parse:', $LF, $text;
    return $parser->tptp_input ($text);
}

sub parse_fof_formula {
    my $text = shift;
    warn 'trying to parse:', $LF, $text;
    return $parser->fof_formula ($text);
}

sub make_formula {
    my $formula_string = shift;
    return parse_tptp_formula ($formula_string);
}

sub tptpify {
    my $self = shift;
    my $kind = $self->get_kind ();
    my $name = $self->get_name ();
    my $status = $self->get_status ();
    my $content = $self->get_formula ();
    return "${kind}(${name},${status},${content}).";
}

sub change_status {
    my $self = shift;
    my $new_status = shift;

    if (! defined $new_status) {
	croak 'To change the status of a formula, one must supply a new status.';
    }

    if ($new_status eq $EMPTY_STRING) {
	croak 'The empty string is not an acceptable formula status.';
    }

    if ($new_status =~ / \N{LF} /m) {
	croak 'A TPTP formula status cannot contain a linefeed character.';
    }

    if ($new_status =~ / \s /) {
	croak 'A TPTP formula status cannot contain whitespace.';
    }

    if ($new_status !~ / [a-z_]+ /) {
	croak 'A TPTP formula status must consist of lowercase alphabetic characters and the underscore character \'_\'.';
    }

    my $formula = $self->get_formula ();
    my $name = $self->get_name ();
    my $kind = $self->get_kind ();

    return Formula->new (kind => $kind,
			 status => $new_status,
			 name => $name,
			 formula => $formula);
}

sub make_axiom {
    my $self = shift;
    return $self->change_status ('axiom');
}

sub make_conjecture {
    my $self = shift;
    return $self->change_status ('conjecture');
}

sub negate {
    my $self = shift;

    my $formula = $self->get_formula ();

    return Formula->new (
	kind => $self->get_kind (),
	status => $self->get_status (),
	name => $self->get_name (),
	formula => "~ ( ${formula} )",
    );

}

sub name_with_color {
    my $self = shift;
    my $color = shift;

    my $name = $self->get_name ();
    return colored ($name, $color);
}

sub equal_to {
    my $self = shift;
    my $other_formula = shift;

    my $self_kind = $self->get_kind ();
    my $self_name = $self->get_name ();
    my $self_status = $self->get_status ();
    my $self_formula = $self->get_formula ();

    my $other_formula_kind = $other_formula->get_kind ();
    my $other_formula_name = $other_formula->get_name ();
    my $other_formula_status = $other_formula->get_status ();
    my $other_formula_formula = $other_formula->get_formula ();

    return ($self_kind eq $other_formula_kind
		&& $self_name eq $other_formula_name
		    && $self_status eq $other_formula_status
			&& $self_formula eq $other_formula_formula);
}

sub is_first_order {
    my $self = shift;
    my $kind = $self->get_kind ();

    return $kind ne 'tff';
}

# Module implementation here

sub parse_atomic_formula {
    my $text = shift;
    return $parser->atomic_formula ($text);
}

sub parse_tptp_file {
    my $path = shift;
    my $content = slurp ($path);
    # warn 'Trying to parse:', $LF, $content;
    my $formulas_ref = $parser->tptp_file ($content);
    if (! defined $formulas_ref) {
        confess 'Unable to parse:', $LF, $content;
    }
    my @formulas = @{$formulas_ref};
    warn 'Parsed formulas:', $LF, Dumper (@formulas);
    return @formulas;
}

1;
__END__

=pod

=head1 NAME

=head1 DESCRIPTION

Formula is a package (specifically, a Moose class) that contains some
functionality for working with TPTP formulas.

=head1 DEPENDENCIES

=over 8

=item L<Moose|http://search.cpan.org/~doy/Moose-2.0403/lib/Moose.pm>

=back

=head1 SEE ALSO

=over 8

=item L<Getting started with TPTP and TSTP/http://www.cs.miami.edu/~tptp/TPTP/QuickGuide/>

One can see here how TPTP formulas are specified and used.

=back

=cut
