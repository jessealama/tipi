#!/usr/bin/perl -w

use strict;

my $within_formula = 0;

while (defined (my $line = <STDIN>)) {
  chomp $line;
  if ($line =~ /^fof\(/) { # beginning of a formula
    $within_formula = 1;
  }
  if ($line =~ /---------/) { # we're done with the whole proof
    $within_formula = 0;
  }
  if ($line =~ /^%/) { # TPTP comment
    $within_formula = 0;
  }
  if ($within_formula == 1) {
    print $line, "\n";
  }
}
