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
	     apply_stylesheet
             is_readable_file
             slurp);

our @EXPORT_OK = qw(parse_tptp_formula parse_fof_formula parse_tptp_file is_implication);

# Strings
Readonly my $TWO_SPACES => q{  };
Readonly my $EMPTY_STRING => q{};
Readonly my $LF => "\N{LF}";

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

sub is_implication {
    my $thing = shift;
    return 0;
}

Readonly my $TPTP_GRAMMAR_AUTOTREE =>
    q {<autotree>
          tptp_file: tptp_input(s?)
          tptp_input: annotated_formula | include | comment | <error>
          comment: /[%].*/
          annotated_formula: fof_annotated
          fof_annotated: fof_keyword comment(s?) left_paren comment(s?) name comment(s?) comma comment(s?) formula_role comment(s?) comma comment(s?) fof_formula comment(s?) right_paren comment(s?) full_stop
          fof_annotated: fof_keyword comment(s?) left_paren comment(s?) name comment(s?) comma comment(s?) formula_role comment(s?) comma comment(s?) fof_formula comment(s?) comma comment(s?) source comment(s?) right_paren comment(s?) full_stop
          fof_annotated: fof_keyword comment(s?) left_paren comment(s?) name comment(s?) comma comment(s?) formula_role comment(s?) comma comment(s?) fof_formula comment(s?) comma comment(s?) source comment(s?) comma comment(s?) optional_info comment(s?) right_paren comment(s?) full_stop
          fof_keyword: 'fof'
          left_paren: '('
          right_paren: ')'
          full_stop: '.'
          comma: ','
          colon: ':'
          left_bracket: '['
          right_bracket: ']'
          source: dag_source | internal_source | external_source | unknown_keyword
          unknown_keyword: 'unknown'
          source: left_bracket comment(s?) sources comment(s?) right_bracket
          sources: source comment(s?) comma comment(s?) sources
          sources: source
          dag_source: name | inference_record
          inference_record: inference_keyword comment(s?) left_paren comment(s?) inference_rule comment(s?) comma comment(s?) useful_info comment(s?) comma comment(s?) inference_parents comment(s?) right_paren
          inference_keyword: 'inference'
          inference_parents: left_bracket parent_list right_bracket
          parent_list: parent_info comma parent_list
          parent_list: parent_info
          parent_info: source parent_details
          parent_details: colon general_list
          parent_details: ''
          inference_parents: left_bracket right_bracket
          inference_rule: atomic_word
          useful_info: general_list
          general_list: left_bracket comment(s?) right_bracket
          general_list: left_bracket comment(s?) general_terms comment(s?) right_bracket
          general_terms: general_term comment(s?) comma comment(s?) general_terms
          general_terms: general_term
          general_term: general_data
          general_term: general_data comment(s?) colon comment(s?) general_term
          general_term: general_list
          general_data: atomic_word | general_function | variable | number | distinct_object | formula_data
          general_function: atomic_word comment(s?) left_paren comment(s?) general_terms comment(s?) right_paren
          optional_info: comma comment(s?) useful_info
          optional_info: ''
          internal_source: introduced_keyword comment(s?) left_paren comment(s?) intro_type comment(s?) optional_info comment(s?) right_paren
          intro_type: 'definition' | 'axiom_of_choice' | 'tautology' | 'assumption'
          introduced_keyword: 'introduced'
          external_source: file_source | theory | creator_source
          file_source: file_keyword left_paren file_name file_info right_paren
          file_keyword: 'file'
          file_info: comma name
          file_info: ''
          theory: theory_keyword left_paren theory_name optional_info right_paren
          theory_keyword: 'theory'
          theory_name: 'equality' | 'ac'
          creator_source: creator_keyword comment(s?) left_paren comment(s?) creator_name comment(s?) optional_info comment(s?) right_paren
          creator_keyword: 'creator'
          creator_name: atomic_word
          name: atomic_word
          name: integer
          atomic_word: /[a-z][a-zA-Z0-9_]*/
          lower_alpha: /[a-z]/
          upper_alpha: /[A-Z]/
          numeric: /[0-9]/
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
          positive_decimal: /[1-9][0-9]*/
          non_zero_numeric: /[1-9]/
          real: signed_real | unsigned_real
          signed_real: sign unsigned_real
          unsigned_real: left_paren decimal_fraction right_paren
          unsigned_real: left_paren  decimal_exponent right_paren
          decimal_fraction: decimal dot_decimal
          dot_decimal: dot numeric(s)
          dot: '.'
          decimal_exponent: '(' decimal ')' exponent integer
          decimal_exponent: '(' decimal_fraction ')' exponent integer
          exponent: /[Ee]/
          positive_decimal: /[1-9]/ numeric(s?)
          formula_role: 'axiom' | 'hypothesis' | 'definition' | 'assumption' | 'lemma' | 'theorem' | 'conjecture' | 'negated_conjecture' | 'plain' | 'fi_domain' | 'fi_functors' | 'fi_predicates' | 'type' | 'unknown'
          fof_formula: fof_logic_formula | fof_sequent
          fof_sequent: fof_tuple gentzen_arrow fof_tuple
          fof_sequent: left_paren fof_sequent right_paren
          fof_tuple: left_bracket fof_tuple_list right_bracket
          fof_tuple: left_bracket right_bracket
          fof_tuple_list: fof_logic_formula comma fof_tuple_list
          fof_tuple_list: fof_logic_formula
          gentzen_arrow: '-->'
          fof_logic_formula: fof_binary_formula | fof_unitary_formula
          fof_binary_formula: fof_binary_nonassoc | fof_binary_assoc
          fof_binary_assoc: fof_or_formula | fof_and_formula
          vline: '|'
          ampersand: '&'
          fof_or_formula: fof_unitary_formula vline fof_unitary_formula
          fof_or_formula: fof_unitary_formula vline fof_or_formula
          fof_and_formula: fof_unitary_formula ampersand fof_unitary_formula
          fof_and_formula: fof_unitary_formula ampersand fof_and_formula
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
          fof_unitary_formula: left_paren fof_logic_formula right_paren
          fof_unitary_formula: atomic_formula | fof_quantified_formula | fof_unary_formula
          fof_quantified_formula: fol_quantifer '[' fof_variable_list ']' ':' fof_unitary_formula
          fol_quantifer: '!' | '?'
          fof_variable_list: variable ',' fof_variable_list | variable
          variable: upper_word
          upper_word: /[A-Z][a-zA-Z0-9_]*/
          single_quoted: single_quote sq_char(s) single_quote
          sq_char: /[a-zA-Z0-9.]+/
          sq_char: "/"
          sq_char: '\\''
          do_char: /[a-zA-Z0-9]/
          do_char: /[\]["]/
          single_quote: "'"
          fof_unary_formula: unary_connective fof_unitary_formula | fol_infix_unary
          unary_connective: '~'
          fol_infix_unary: term infix_inequality term
          infix_inequality: '!='
          term: variable | function_term
          function_term: plain_term # | defined_term | system_term
          plain_term: functor left_paren arguments right_paren
          plain_term: constant
          functor: atomic_word
          constant: functor
          arguments: term comma arguments
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
          dollar_dollar_word: /[$][$][a-z][a-zA-Z0-9_]*/
          atomic_formula: plain_atomic_formula # | defined_atomic_formula | system_atomic_formula
          plain_atomic_formula: plain_term
          defined_atomic_formula: defined_plain_formula | defined_infix_formula
          defined_plain_formula: defined_plain_term
          defined_infix_formula: term defined_infix_pred term
          defined_infix_pred: infix_equality
          infix_equality: '='
          system_atomic_formula: system_term
          include: include_keyword left_paren file_name formula_selection right_paren full_stop
          include: include_keyword left_paren file_name right_paren full_stop
          include_keyword: 'include'
          file_name: single_quoted
          formula_selection: comma '[' name_list ']'
          name_list: name comma name_list
          name_list: name
  };

# $::RD_TRACE = 1;
# $::RD_HINT = 1;
# $::RD_WARN = 1;

my $skip_whitespace = qr/[ \t\n]*/m;
$Parser::RecDescent::skip = $skip_whitespace;

my $parser = new Parse::RecDescent ($TPTP_GRAMMAR_AUTOTREE)
    or confess 'Bad grammar!';

sub parse_tptp_formula {
    my $text = shift;
    chomp $text;
    my $copy = "${text}";
    my $result = $parser->tptp_input (\$copy);
    if (defined $result) {
        if ($copy eq '') {
            return $result;
        } else {
            confess 'Able to parse only a proper initial segment of', $LF, $text, 'There are ', length $copy, ' characters remaining to be parsed:', $LF, $copy;
        }
    } else {
        confess 'Unable to parse', $LF, $text;
    }
}

sub parse_fof_formula {
    my $text = shift;
    chomp $text;
    my $copy = "${text}";
    my $result = $parser->fof_logic_formula (\$copy);
    if (defined $result) {
        if ($copy eq '') {
            return $result;
        } else {
            confess 'Able to parse only a proper initial segment of', $LF, $text, 'There are ', length $copy, ' characters remaining to be parsed:', $LF, $copy;
        }
    } else {
        confess 'Unable to parse', $LF, $text;
    }
}

sub parse_tptp_file {
    my $path = shift;
    unless (is_readable_file ($path)) {
        confess $path, ' either does not exist or is unreadable.';
    }
    my $content = slurp ($path);
    chomp $content;
    my $parsed = $parser->tptp_file (\$content);
    if ($content ne '') {
        if (defined $parsed) {
            confess $path, ' could be parsed as a TPTP file, but only a proper initial segment of it was parsed.  The unparsed remainder (length ', length $content, '):', $LF, $content;
        } else {
            confess 'Could not parse (the whole of) ', $path, '.';
        }
    }
    return $parsed;
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
