package Utils;

use base qw(Exporter);
use warnings;
use strict;
use Regexp::DefaultFlags;
use Carp qw(croak);

our @EXPORT_OK = qw(ensure_readable_file
		    ensure_directory
		    ensure_executable
	            write_string_to_file
	            extension
		    strip_extension);

sub ensure_readable_file {
  my $file = shift;
  (-e $file && ! -d $file && -r $file) ? return 1
                                       : return 0;
}

sub ensure_executable {
    my $file = shift;
    if (ensure_readable_file ($file)) {
	if (-x $file) {
	    return 1;
	} else {
	    return 0;
	}
    } else {
	return 0;
    }
}

sub ensure_directory {
    my $dir = shift;
    (-d $dir) ? return 1 : return 0;
}

sub write_string_to_file {
    my $string = shift;
    my $path = shift;

    if (! defined $string) {
	croak ('Error: please supply a string.');
    }

    if (! defined $path) {
	croak ('Error: please supply a path.');
    }

    open (my $fh, '>', $path)
	or croak ('Error: unable to open an output filehandle to ', $path, ': ', $!);
    print {$fh} ($string)
	or croak ('Error: unable to print the given string to the given path: ', $!);
    close $fh
	or croak ('Error: unable to close the output filehandle to ', $path, ': ', $!);

    return 1;
}

sub extension {
  my $path = shift;
  if ($path =~ /[.]([^.]+)$/) {
    return $1;
  } else {
    croak ('Error: the path \'', $path, '\' does not have an extension.');
  }
}

sub strip_extension {
    my $path = shift;

    if ($path =~ /\A (.+) [.] [^.]* \z/) {
	return $1;
    } elsif ($path =~ / [^.]+ /) {
	return $path;
    } else {
	# Seems logically impossible
	croak ('Error: the path \'', $path, '\' is too weird.');
    }

}

1;
