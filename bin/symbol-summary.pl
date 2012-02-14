#!/usr/bin/perl -w

use strict;

use POSIX qw(floor);

use Term::ANSIColor;

# swiped from the perldocs for log
sub log10 {
  my $n = shift;
  return log($n)/log(10);
}

sub copy_string {
  my $string = shift;
  my $num_copies = shift;
  my $final_string = '';
  foreach my $i (1 .. $num_copies) {
    $final_string .= $string;
  }
  return $final_string;
}

if (scalar @ARGV == 0) {
  print 'Usage: symbol-summary.pl TPTP-FILE';
  exit 1;
}

if (scalar @ARGV > 1) {
  print 'Usage: symbol-summary.pl TPTP-FILE';
  exit 1;
}

my $tptp_file = $ARGV[0];

# Sanity checks: the file exists, is readable, we have tptp4X, the input file is a sane TPTP file
# according to tptp4X, and we have GetSymbols

if (! -e $tptp_file) {
  print "Error: there is no file at '$tptp_file'.";
  exit 1;
}

if (! -r $tptp_file) {
  print "Error: the file at '$tptp_file' is unreadable.";
  exit 1;
}

my $which_tptp4X = system ('which tptp4X > /dev/null 2>&1');
my $which_tptp4X_exit_code = $which_tptp4X >> 8;

if ($which_tptp4X_exit_code != 0) {
  print "Error: we are missing the required tptp4X tool.  Is it in your PATH?\n";
  exit 1;
}

my $tptp4X_result = system ("tptp4X -N -V -c -x -umachine $tptp_file > /dev/null 2>&1");
my $tptp4X_exit_code = $tptp4X_result >> 8;

if ($tptp4X_exit_code != 0) {
  print "Error: the file at '$tptp_file' is not a valid TPTP file.\n";
  exit 1;
}

my $which_GetSymbols_result = system ('which GetSymbols > /dev/null 2>&1');
my $which_GetSymbols_exit_code = $which_GetSymbols_result >> 8;

if ($which_GetSymbols_exit_code != 0) {
  print "Error: we are missing the required GetSymbols tool.  Is it in your PATH?\n";
  exit 1;
}

my @GetSymbols_result = `tptp4X -N -V -c -x -umachine $tptp_file | GetSymbols -all -- 2> /dev/null`;

chomp @GetSymbols_result;

if (scalar @GetSymbols_result == 0) {
  print STDERR ("Warning: GetSymbols reported that there were no symbols in '$tptp_file'.\n");
  exit 0;
}

if (scalar @GetSymbols_result > 1) {
  print STDERR ("Error: GetSymbols produced multiple lines of output, which we were not expecting.\n");
  exit 1;
}

my $getsymbols_result = $GetSymbols_result[0];

$getsymbols_result =~ m/symbols\(all,\[(.+)\],\[(.+)\]\)\.$/;

(my $function_symbols_list, my $predicate_symbols_list) = ($1, $2);

unless (defined $function_symbols_list and defined $predicate_symbols_list) {
  print STDERR ("Error: something went wrong parsing the output\n\n  $getsymbols_result\n\nof GetSymbols\n");
  exit 1;
}

my @function_symbol_infos = split /,/, $function_symbols_list;
my @predicate_symbol_infos = split /,/, $predicate_symbols_list;

sub symbol_only {
  my $symbol_info = shift;
  if (defined $symbol_info) {
    (my $symbol) = split /\//, $symbol_info;
    return $symbol;
  } else {
    print "Warning: symbol_only got an undefined argument!";
    return "";
  }
}

my @all_infos = ();
push (@all_infos, @function_symbol_infos);
push (@all_infos, @predicate_symbol_infos);

my @all_symbols = map  { symbol_only($_) } @all_infos;

my $length_of_longest_symbol_name = 0;

foreach my $symbol (@all_symbols) {
  my $len = length $symbol;
  if ($len > $length_of_longest_symbol_name) {
    $length_of_longest_symbol_name = $len;
  }
}

my $padding = abs (length ('Predicate Symbol') - $length_of_longest_symbol_name);

if (scalar @function_symbol_infos == 0 && scalar @predicate_symbol_infos == 0) {
  print "Warning: there were no function symbols and no predicate symbols.  Is this really a sensible TPTP theory?";
}

if (scalar @function_symbol_infos > 0) {
  print 'Function Symbol', copy_string (' ', $padding + 1), ' | Arity | # Occurrences', "\n";
  print '===============', copy_string ('=', $padding + 1), '=|=======|==============', "\n";
# difference b/w length of 'Predicate' and 'Function' ^^^

  foreach my $symbol_info (sort @function_symbol_infos) {
    $symbol_info =~ m/(.+)\/([0-9]+)\/([0-9]+)/;
    (my $symbol, my $arity, my $num_occurrences) = ($1, $2, $3);
    unless (defined $symbol and defined $arity and defined $num_occurrences) {
      print STDERR ("Error: unable to parse the function symbol info '$symbol_info'");
      exit 1;
    }
    my $length_of_this_symbol = length $symbol;
    if ($num_occurrences == 1) {
      print colored ($symbol, 'red'), copy_string (' ', $length_of_longest_symbol_name - $length_of_this_symbol), ' |   ', $arity, '   | ', $num_occurrences, "\n";
    } else {
      print $symbol, copy_string (' ', $length_of_longest_symbol_name - $length_of_this_symbol), ' |   ', $arity, '   | ', $num_occurrences, "\n";
    }
  }
}

if (scalar @predicate_symbol_infos > 0) {
  if (scalar @function_symbol_infos > 0) {
    print "\n"; # to separate the predicate symbol report from the function symbol report
  }
  print 'Predicate Symbol', copy_string (' ', $padding), ' | Arity | # Occurrences', "\n";
  print '================', copy_string ('=', $padding), '=|=======|==============', "\n";
  foreach my $symbol_info (sort @predicate_symbol_infos) {
    $symbol_info =~ m/(.+)\/([0-9]+)\/([0-9]+)/;
    (my $symbol, my $arity, my $num_occurrences) = ($1, $2, $3);
    unless (defined $symbol and defined $arity and defined $num_occurrences) {
      print STDERR ("Error: unable to parse the predicate symbol info '$symbol_info'");
      exit 1;
    }
    my $length_of_this_symbol = length $symbol;
    if ($num_occurrences == 1) {
      print colored ($symbol, 'red'), copy_string (' ', $length_of_longest_symbol_name - $length_of_this_symbol), ' |   ', $arity, '   | ', $num_occurrences, "\n";
    } else {
      print $symbol, copy_string (' ', $length_of_longest_symbol_name - $length_of_this_symbol), ' |   ', $arity, '   | ', $num_occurrences, "\n";
    }
  }
}

exit 0;
