#!/usr/bin/perl -w

# Feed out standard input to epclextract until we encounter an input
# line that looks like
#
#   'SZS status CounterSatisfiable'
#
# at which point we exit uncleanly.

use strict;

# launch epclextract

open (EPCLEXTRACT, '|epclextract')
  or (print STDERR 'Error: we are unable to open a pipe to epclextract.' && exit 1);

while (defined (my $line = <STDIN>)) {
  if ($line =~ /SZS status CounterSatisfiable/) {
    close EPCLEXTRACT
      or (print STDERR 'Error: our problem is countersatisfiable, and we wish to close our pipe to epclextract, but we are unable to close the pipe.' && exit 1);
    exit 2;
  } else {
    print EPCLEXTRACT ($line);
  }
}

close EPCLEXTRACT
  or (print STDERR 'Error: we are unable to close the pipe to epclextract.' && exit 1);

exit 0;
