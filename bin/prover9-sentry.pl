#!/usr/bin/perl -w

# Copy standard input to prooftrans until we encounter an input line
# that looks like
#
#   'exit (sos empty)'
#
# at which point we exit uncleanly.  If we never encounter such a
# line, terminate cleanly.

use strict;

# launch prooftrans

open (PROOFTRANS, '|prooftrans')
  or (print STDERR 'Error: we are unable to open a pipe to prooftrans.' && exit 1);

while (defined (my $line = <STDIN>)) {
  if ($line =~ /exit \(sos empty\)/) {
    close PROOFTRANS
      or (print STDERR 'Error: our problem is countersatisfiable, so we wish to close our pipe to prooftrans, but we cannot.' && exit 1);
    exit 2;
  } else {
    print PROOFTRANS ($line);
  }
}

close PROOFTRANS
  or (print STDERR 'Error: we are unable to close the pipe to echo.' && exit 1);

exit 0;
