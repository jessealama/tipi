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
use List::MoreUtils qw(first_index any all none);

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
		    all_sublists
		    all_nonempty_sublists
		    remove_duplicate_lists
		    subtuple
		    tuple_less_than_wrt_ordering
	            message_with_extra_linefeed
		    asterisk_list
		    sublist
		    list_member);

Readonly my $EMPTY_STRING => q{};
Readonly my $BAD_COLOR => 'red';
Readonly my $SPACE => q{ };
Readonly my $TWO_SPACES => q{  };
Readonly my $FULL_STOP => q{.};
Readonly my $ASTERISK => '*';
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

sub maybe_render_undefined {
    my $str = shift;
    return defined $str ? $str : '(undefined)';
}

sub message {
    my @message_parts = @_;

    @message_parts = map { maybe_render_undefined ($_) } @message_parts;

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
    }

    foreach my $i (0 .. $len - 1) {
	my $a = $l[$i];
	my @tail = @l[$i + 1 .. $len - 1];
	my @all_from_tail = all_sublists (\@tail);
	my @extended = prepend_element_to_all_lists ($a, \@all_from_tail);
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

    my @extended = ();

    foreach my $list_ref (@list_of_list_refs) {
	my @new = ();
	push (@new, $element);
	my @l = @{$list_ref};
	push (@new, @l);
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
    }

    foreach my $i (0 .. $len - 1) {
	my $a = $l[$i];
	my @tail = @l[$i + 1 .. $len - 1];
	my @all_from_tail = all_sublists (\@tail);
	my @extended = prepend_element_to_all_lists ($a, \@all_from_tail);
	foreach my $lst_ref (@extended) {
	    my @lst = @{$lst_ref};
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

sub list_member {
    my $element = shift;
    my $list_ref = shift;
    my @list = @{$list_ref};
    return ((any { $_ eq $element } @list) ? 1 : 0);
}

sub sublist {
    my $list_ref_1 = shift;
    my $list_ref_2 = shift;

    my @list_1 = @{$list_ref_1};

    if (scalar @list_1 == 0) {
	return 1;
    } else {
	my $result = (all { list_member ($_, $list_ref_2) } @list_1);
	$result = $result ? 1 : 0;
	return $result;
    }


}

sub equal_lists {
    my $list_ref_1 = shift;
    my $list_ref_2 = shift;

    my @list_1 = @{$list_ref_1};
    my @list_2 = @{$list_ref_2};

    if (sublist ($list_ref_1, $list_ref_2)) {
	return sublist ($list_ref_2, $list_ref_1);
    } else {
	return 0;
    }

}

sub remove_duplicate_lists {
    my @list_refs = @_;

    my $num_list_refs = scalar @list_refs;
    my @final_list = ();

    foreach my $i (0 .. $num_list_refs - 1) {
	my $list_ref = $list_refs[$i];
	my @tail = @list_refs[$i + 1 .. $num_list_refs - 1];
	if (scalar @tail == 0) {
	    push (@final_list, $list_ref);
	} else {
	    if (none { equal_lists ($_, $list_ref) } @tail) {
		push (@final_list, $list_ref);
	    }
	}
    }

    if (wantarray) {
	return @final_list;
    } else {
	return \@final_list;
    }
}

sub asterisk_list {
    my @items = @_;
    my $list = $EMPTY_STRING;
    foreach my $item (@items) {
	$list .= $TWO_SPACES . $ASTERISK . $SPACE . $item . "\N{LF}";
    }
    return $list;
}

1;
__END__

=pod

=cut
