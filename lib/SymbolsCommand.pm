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
	    ensure_getsymbols_available
	    tptp4X_output);
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

sub BUILD {
    my $self = shift;
    $self->_set_description ($DESCRIPTION);
    return $self;
}

around 'execute' => sub {
    my $orig = shift;
    my $self = shift;
    my @arguments = @_;

    GetOptionsFromArray (
	\@arguments,
	'man' => \$opt_man,
	'verbose' => \$opt_verbose,
	'help|?' => \$opt_help,
	'debug' => \$opt_debug,
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

    if (! ensure_sensible_tptp_theory ($theory_path)) {
	my $errors = tptp4X_output ($theory_path);
	say {*STDERR} error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	say {*STDERR} 'Here is what tptp4X output when evaluating the file:';
	say {*STDERR} $errors;
	exit 1;
    }

    return $self->$orig (@arguments);

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

	my @all_symbols = $theory->get_all_symbols ();
	my @all_symbols_sorted = sort @all_symbols;

	my @records = ();

	foreach my $symbol (@all_symbols_sorted) {

	    my $count_for_symbol = undef;

	    if (defined $function_symbol_counts{$symbol}) {
		$count_for_symbol = $function_symbol_counts{$symbol};
	    } elsif (defined $predicate_symbol_counts{$symbol}) {
		$count_for_symbol = $predicate_symbol_counts{$symbol};
	    } else {
		croak 'Unknown symbol \'', $symbol, '\'.';
	    }

	    my %formulas_for_symbol = ();
	    my $num_occurrences = 0;

	    foreach my $formula (@formula_names) {
		if (defined $function_table{$formula}) {
		    my %by_symbol = %{$function_table{$formula}};
		    if (defined $by_symbol{$symbol}) {
			my $num_occurrences_here = $by_symbol{$symbol};
			$num_occurrences += $num_occurrences_here;
			$formulas_for_symbol{$formula} = $num_occurrences_here;
		    }
		}

		if (defined $predicate_table{$formula}) {
		    my %by_symbol = %{$predicate_table{$formula}};
		    if (defined $by_symbol{$symbol}) {
			my $num_occurrences_here = $by_symbol{$symbol};
			$num_occurrences += $num_occurrences_here;
			$formulas_for_symbol{$formula} = $num_occurrences_here;
		    }
		}

	    }

	    my @record = ($symbol,
			  $num_occurrences,
			  \%formulas_for_symbol);
	    push (@records, \@record);

	}

	$self->print_symbols_by_occurrence (\@records);

    }

    return 1;

}

sub print_symbols_by_occurrence {
    my $self = shift;
    my $records_ref = shift;

    my @records = @{$records_ref};

    my $symbol;
    my $num_total_occurrences;

    format STDOUT_TOP =
@<<<<<<<<<<<<<<<... @<<<<<<<<<<<<<<<<<<<<<<<<<<<
'Symbol',           'Total number of occurrences'
.

    format STDOUT =
@<<<<<<<<<<<<<<<... @*
$symbol,            $num_total_occurrences
.

	# write STDOUT_TOP;
    foreach my $record_ref (@records) {
	my @record = @{$record_ref};

	if ($opt_debug) {
	    warn 'Record is: ', Dumper (@record);
	}

	$symbol = $record[0];
	$num_total_occurrences = $record[1];
	if ($num_total_occurrences == 1) {
	    $num_total_occurrences = colored ($num_total_occurrences, 'red');
	}
	write;
    }

    return;

}

1;
__END__
