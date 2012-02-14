#!/usr/bin/perl -w

# Copy standard input to standard output until we encounter an input
# line that looks like
#
#   'SZS status CounterSatisfiable'
#
# at which point we exit uncleanly.  If we never encounter such a
# line, terminate cleanly.

use strict;

my $countersatisfiable = undef;
my $satisfiable = undef;
my $timeout = undef;

while (defined (my $line = <STDIN>)) {
  print $line;
  if ($line =~ /SZS status CounterSatisfiable/) {
    $countersatisfiable = 0;
  }
  if ($line =~ /SZS status Satisfiable/) {
    $satisfiable = 0;
  }
  if ($line =~ /SZS status Timeout/) {
    $timeout = 0;
  }
}

if (defined $countersatisfiable) {
  exit 2;
} elsif (defined $timeout) {
  exit 3;
} elsif (defined $satisfiable) {
  exit 4;
} else {
  exit 1;
}
