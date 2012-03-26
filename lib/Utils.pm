package Utils;

use base qw(Exporter);
use warnings;
use strict;
use Regexp::DefaultFlags;
use Carp qw(croak);
use Readonly;
use charnames qw(:full);
use English qw(-no_match_vars);
use Term::ANSIColor;
use Data::Dumper;
use List::MoreUtils qw(first_index);

our @EXPORT_OK = qw(ensure_readable_file
		    ensure_directory
		    ensure_executable
	            write_string_to_file
	            extension
		    strip_extension
		    slurp
		    error_message
		    warning_message
		    message
		    all_nonempty_sublists
		    subtuple
		    tuple_less_than_wrt_ordering);

Readonly my $EMPTY_STRING => q{};
Readonly my $BAD_COLOR => 'red';
Readonly my $SPACE => q{ };
Readonly my $FULL_STOP => q{.};
Readonly my @EMPTY_LIST => ();

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

sub slurp {
    my $path_or_fh = shift;

    open (my $fh, '<', $path_or_fh)
	or die 'Error: unable to open the file (or filehandle) ', $path_or_fh, '.';

    my $contents;
    { local $/; $contents = <$fh>; }

    close $fh
	or die 'Error: unable to close the file (or filehandle) ', $path_or_fh, '.';

    return $contents;
}

sub message {
    my @message_parts = @_;
    my $msg = join $EMPTY_STRING, @message_parts;
    return $msg . "\N{LF}";
}

sub message_with_extra_linefeed {
    my @message_parts = @_;
    my $msg = join $EMPTY_STRING, @message_parts;
    return message (message ($msg));
}

sub warning_message {
    my @message_parts = @_;
    my $message = join ($EMPTY_STRING, @message_parts);
    my $message_with_warning_padding
	= colored ('Warning', $BAD_COLOR) . ': ' . $message;
    return $message_with_warning_padding;
}

sub error_message {
    my @message_parts = @_;
    my $message = join ($EMPTY_STRING, @message_parts);
    my $message_with_error_padding = colored ('Error', $BAD_COLOR) . ': ' . $message;
    return $message_with_error_padding;
}

sub all_sublists {
    my $l_ref = shift;
    my @l = @{$l_ref};

    my $len = scalar @l;

    my @empty = ();
    my @all = ();

    push (@all, \@empty);

    if ($len == 0) {
	if (wantarray) {
	    return @all;
	} else {
	    return \@all;
	}
    } else {
	warn 'Computing all sublists of the', $SPACE, scalar @l, '-element list', $SPACE, join (',', @l);
    }

    foreach my $i (0 .. $len - 1) {
	my $a = $l[$i];
	my @tail = @l[$i + 1 .. $len - 1];
	warn 'Tail has length', $SPACE, scalar @tail, $FULL_STOP;
	my @all_from_tail = all_sublists (\@tail);
	warn 'Recursively, we got', $SPACE, scalar @all_from_tail, $SPACE, 'new lists.';
	my @extended = prepend_element_to_all_lists ($a, \@all_from_tail);
	warn '@extended (all_sublists) is', $SPACE, Dumper (@extended);
	foreach my $lst (@extended) {
	    push (@all, $lst);
	}
    }

    if (wantarray) {
	return @all;
    } else {
	return \@all;
    }

}

sub prepend_element_to_all_lists {
    my $element = shift;
    my $list_of_lists_ref = shift;

    my @list_of_list_refs = @{$list_of_lists_ref};

    warn 'prepend_element_to_all_lists: prepend', $SPACE, $element, ' to every list in the', $SPACE, scalar @list_of_list_refs, '-element list of lists', "\N{LF}", Dumper (@list_of_list_refs);

    my @extended = ();

    foreach my $list_ref (@list_of_list_refs) {
	my @new = ();
	push (@new, $element);
	my @l = @{$list_ref};
	warn 'adding ', $element, ' to the', $SPACE, scalar @l, '-element list ', Dumper (@l);
	push (@new, @l);
	warn 'Done adding ', $element, ' to the list ', Dumper (@l), '; the result is ', Dumper (@new);
	push (@extended, \@new);
    }

    if (wantarray) {
	return @extended;
    } else {
	return \@extended;
    }

}

sub all_nonempty_sublists {
    my $l_ref = shift;
    my @l = @{$l_ref};

    my $len = scalar @l;

    my @all = ();

    if ($len == 0) {
	return \@all;
    } else {
	warn 'Computing all nonempty sublists of the', $SPACE, scalar @l, '-element list', $SPACE, join (',', @l);
    }

    foreach my $i (0 .. $len - 1) {
	my $a = $l[$i];
	my @tail = @l[$i + 1 .. $len - 1];
	warn 'Tail has length', $SPACE, scalar @tail, $FULL_STOP;
	my @all_from_tail = all_sublists (\@tail);
	warn 'From all_sublists, we got', $SPACE, scalar @all_from_tail, $SPACE, 'new lists:', $SPACE, Dumper (@all_from_tail);
	my @extended = prepend_element_to_all_lists ($a, \@all_from_tail);
	warn '@extended (all_nonempty_sublists) is:', $SPACE, Dumper (@extended);
	foreach my $lst_ref (@extended) {
	    my @lst = @{$lst_ref};
	    warn 'An extended list:', $SPACE, Dumper (@lst);
	    push (@all, $lst_ref);
	}
    }

    if (wantarray) {
	return @all;
    } else {
	return \@all;
    }
}

sub subtuple {
    my $aref_1 = shift;
    my $aref_2 = shift;
    my @array_1 = @{$aref_1};
    my @array_2 = @{$aref_2};

    warn 'Checking whether', "\N{LF}", join ("\N{SPACE}", @array_1), "\N{LF}", 'is a subtuple of', "\N{LF}", join ("\N{SPACE}", @array_2);

    if (scalar @array_1 == 0) {
	return 1;
    } elsif (scalar @array_1 <= scalar @array_2) {
	my $a_1_head = $array_1[0];
	my $a_1_head_idx = first_index { $_ eq $a_1_head } @array_2;
	if ($a_1_head_idx < 0) {
	    return 0;
	} else {
	    my @a_1_tail = @array_1[1 .. scalar @array_1 - 1];
	    my @a_2_tail = @array_2[$a_1_head_idx + 1 .. scalar @array_2 - 1];
	    return subtuple (\@a_1_tail, \@a_2_tail);
	}
    } else {
	return 0;
    }
}

sub tuple_less_than_wrt_ordering {
    my $a_ref = shift;
    my $b_ref = shift;
    my $ordering_ref = shift;

    my @a = @{$a_ref};
    my @b = @{$b_ref};
    my @ordering = @{$ordering_ref};

    my $a_length = scalar @a;
    my $b_length = scalar @b;

    if ($a_length < $b_length) {
	return -1;
    } elsif ($a_length == $b_length) {
	if ($a_length == 0) {
	    return -1;
	} else {
	    my $a_head = $a[0];
	    my $b_head = $b[0];
	    my $a_index = first_index { $_ eq $a_head } @ordering;
	    my $b_index = first_index { $_ eq $b_head } @ordering;
	    if ($a_index < $b_index) {
		return -1;
	    } elsif ($a_index == $b_index) {
		my @a_tail = @a[1 .. scalar @a - 1];
		my @b_tail = @b[1 .. scalar @b - 1];
		return tuple_less_than_wrt_ordering (\@a_tail,
						     \@b_tail,
						     \@ordering);
	    } else {
		return 1;
	    }
	}
    } else {
	return 1;
    }
}

1;
__END__
