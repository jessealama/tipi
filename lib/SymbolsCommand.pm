package SymbolsCommand;

require v5.10;

use Moose;
use Carp qw(croak carp);
use Pod::Usage;
use Readonly;
use Getopt::Long qw(GetOptionsFromArray :config gnu_compat);
use charnames qw(:full);
use English qw(-no_match_vars);
use Data::Dumper;
use Term::ANSIColor qw(colored);
use feature 'say';
use List::Util qw(max);
use POSIX qw(floor);

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    ensure_getsymbols_available);
use Utils qw(error_message);

Readonly my $DESCRIPTION => 'Print the symbols occurring in a TPTP theory.';
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $EMPTY_STRING => q{};
Readonly my $MAX_SYMBOL_COUNT_DIGITS => 5; # if you are dealing with more than 99999 symbols, you're in trouble

my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_by_occurrence = 1;
my $opt_by_name = 0;

around 'execute' => sub {
    my $orig = shift;
    my $self = shift;
    my @arguments = @_;

    GetOptionsFromArray (
	\@arguments,
	'man' => \$opt_man,
	'verbose' => \$opt_verbose,
	'help|?' => \$opt_help,
    ) or pod2usage (2);

    if ($opt_help) {
        pod2usage(1);
    }

    if ($opt_man) {
        pod2usage(
            -exitstatus => 0,
            -verbose    => 2
        );
    }

    # debug implies verbose
    if ($opt_debug) {
        $opt_verbose = 1;
    }

    if (scalar @arguments == 0) {
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2);
    }

    if (scalar @arguments > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the prove arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
		   -exitval => 2);
    }

    if ($opt_by_occurrence && $opt_by_name) {
	pod2usage (-msg => error_message ('Only one of --by-occurrence and --by-name are permitted.'),
		       -exitval => 2);
    }

    if (! ensure_getsymbols_available ()) {
	say STDERR error_message ('Cannot run GetSymbols');
	exit 1;
    }

    my $theory_path = $arguments[0];

    if (ensure_sensible_tptp_theory ($theory_path)) {
	return $self->$orig (@arguments);
    } else {
	say STDERR error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	exit 1;
    }

};

sub fill_up_to_column {
    my $str = shift;
    my $col = shift;

    return ($str . ($SPACE x ($col - length $str)));
}

sub print_with_initial_padding {
    my $initial_padding = shift;
    my @strings = @_;

    print ($SPACE x $initial_padding);
    foreach my $s (@strings) {
	print $s;
    }

    return 1;

}

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $theory_path = $arguments[0];
    my $theory = Theory->new (path => $theory_path);

    my %predicate_table = %{$theory->get_predicate_symbol_table ()};
    my %function_table = %{$theory->get_function_symbol_table ()};

    # warn 'Predicate table: ', Dumper (%predicate_table);

    my @formulas = $theory->get_formulas (1);


    my @formula_names = map { $_->get_name () } @formulas;

    # warn 'There are ', scalar @formula_names, ' formulas:', "\N{LF}", Dumper (@formula_names);

    if ($opt_by_name) {
	my @sorted_formula_names = sort @formula_names;

	foreach my $formula (@sorted_formula_names) {
	    print $formula;
	    if (defined $predicate_table{$formula}) {

	    }
	    if (defined $function_table{$formula}) {

	    }
	}
    }

    if ($opt_by_occurrence) {

	my %function_symbol_counts = %{$theory->get_function_symbol_counts ()};
	my %predicate_symbol_counts = %{$theory->get_predicate_symbol_counts ()};

	# warn 'Predicate symbol counts table: ', Dumper (%predicate_symbol_counts);

	my @all_symbols = $theory->get_all_symbols ();
	my @all_symbols_sorted = sort @all_symbols;

	my @symbol_lengths = map { length ($_) } @all_symbols;
	my $length_of_longest_symbol = max @symbol_lengths;

	# warn 'All symbols: ', Dumper (@all_symbols_sorted);

	say fill_up_to_column ('Symbol', $length_of_longest_symbol), ' | Number of occurrences | Formulas with occurrences';

	foreach my $symbol (@all_symbols_sorted) {
	    my $current_column = 0;
	    say '----------------------------------------------------------------------';
	    print fill_up_to_column ($symbol, $length_of_longest_symbol);
	    $current_column += $length_of_longest_symbol;
	    print ' | ';
	    $current_column += 3;

	    my $count_for_symbol = undef;

	    if (defined $function_symbol_counts{$symbol}) {
		$count_for_symbol = $function_symbol_counts{$symbol};
	    } elsif (defined $predicate_symbol_counts{$symbol}) {
		$count_for_symbol = $predicate_symbol_counts{$symbol};
	    } else {
		croak 'Unknown symbol \'', $symbol, '\'.';
	    }

	    my $num_digits_of_count = floor (log $count_for_symbol) + 1;

	    # warn 'Number of digits in \'', $count_for_symbol, '\' is ', $num_digits_of_count;

	    print_with_initial_padding ($MAX_SYMBOL_COUNT_DIGITS - $num_digits_of_count,
					$count_for_symbol);

	    $current_column += $MAX_SYMBOL_COUNT_DIGITS - $num_digits_of_count;

	    print ' | ';

	    $current_column += 3;

	    my $printed_first_formula = 0;

	    # warn 'Mapping over ', scalar @formula_names, ' formulas...';
	    foreach my $formula (@formula_names) {
		if (defined $function_table{$formula}) {
		    my %by_symbol = %{$function_table{$formula}};
		    if (defined $by_symbol{$symbol}) {
			my $num_occurrences = $by_symbol{$symbol};
			if ($printed_first_formula) {
			    print_with_initial_padding ($current_column, ' | ', $formula, $SPACE, '(', $num_occurrences, ')', "\N{LF}")
			} else {
			    print $formula, $SPACE, '(', $num_occurrences, ')', "\N{LF}";
			    $printed_first_formula = 1;
			}
		    }
		}

		if (defined $predicate_table{$formula}) {
		    my %by_symbol = %{$predicate_table{$formula}};

		    # warn 'By-symbol table for ', $formula, ' is ', Dumper (%by_symbol);

		    if (defined $by_symbol{$symbol}) {
			my $num_occurrences = $by_symbol{$symbol};
			if ($printed_first_formula) {
			    print_with_initial_padding ($current_column, ' | ', $formula, $SPACE, '(', $num_occurrences, ')', "\N{LF}");

			} else {
			    print $formula, $SPACE, '(', $num_occurrences, ')', "\N{LF}";
			    $printed_first_formula = 1;
			}
		    }
		}
	    }

	}

    }

    return 1;

}

1;
__END__
