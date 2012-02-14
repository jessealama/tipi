#!/usr/bin/perl -w

# Copy standard input to standard output until we encounter an input
# line that looks like
#
#   'Termination reason: Satisfiable'
#
# at which point we exit uncleanly.  If we never encounter such a
# line, terminate cleanly.

use strict;

while (defined (my $line = <STDIN>)) {
  if ($line =~ /Termination reason: Satisfiable/) {
    exit 2;
  } elsif ($line =~ /SZS status Timeout/) {
    exit 3;
  } else {
    print $line;
  }
}

exit 0;
