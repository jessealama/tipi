package VegasCommand;

require v5.10;

use Moose;
use Carp qw(croak carp);
use Pod::Find qw(pod_where);
use Pod::Usage;
use Readonly;
use Getopt::Long qw(GetOptionsFromArray :config gnu_compat);
use charnames qw(:full);
use English qw(-no_match_vars);
use Data::Dumper;
use Term::ANSIColor qw(colored);
use Regexp::DefaultFlags;
use List::Util qw(shuffle max min);
use List::MoreUtils qw(any none first_value first_index);
use Memoize;
use feature 'say';

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    known_prover
	    tptp4X_output);
use Utils qw(error_message
	     ensure_readable_file
	     asterisk_list
	     sublist
	     list_member);
use SZS qw(is_szs_success
	   szs_implies
	   szs_contradicts
	   known_szs_status);

Readonly my $STDIN_PATH => q{--};
Readonly my $TWO_SPACES => q{  };
Readonly my $FULL_STOP => q{.};
Readonly my $SPACE => q{ };
Readonly my $COLON => q{:};
Readonly my $COMMA => q{,};
Readonly my $LF => "\N{LF}";
Readonly my $EMPTY_STRING => q{};
Readonly my $DESCRIPTION => 'Randomly search for a solution.  Play to Win!';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'yellow';
Readonly my $SZS_UNKNOWN => 'Unknown';
Readonly my $SZS_NOT_TRIED => 'NotTried';
Readonly my $SUCCESS => 'success';
Readonly my $WEAKLY_SUCCESSFUL => 'success (weak)';
Readonly my $STRONGLY_UNSUCCESSFUL => 'unsuccessful (strong)';
Readonly my $WEAKLY_UNSUCCESSFUL => 'unsuccessful (weak)';
Readonly my $ASCENDING => 'ascending';
Readonly my $DESCENDING => 'descending';

my $opt_show_output = 0;
my $opt_show_premises = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_solution_szs_status = 'Theorem';
my @opt_provers = ();
my @opt_keep = ();
my $num_to_keep = undef;
my $opt_timeout = 30; # seconds
my $opt_min_subset_size = 0;
my $opt_max_subset_size = undef;

sub print_formula_names_with_color {
    my $formulas_ref = shift;
    my $color = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my @formulas = @{$formulas_ref};

    if (defined $parameters{'sorted'} && $parameters{'sorted'}) {
	my @formula_names = map { $_->get_name () } @formulas;
	my @formula_names_sorted = sort @formula_names;
	my @formula_names_colored
	    = map { colored ($_, $color) } @formula_names_sorted;
	say join ("\N{LF}", @formula_names_colored);
    } else {
	my @formula_names_colored = map { colored ($_, $color) } @formulas;
	say join ("\N{LF}", @formula_names_colored);
    }

    return 1;

}

sub BUILD {
    my $self = shift;
    $self->_set_description ($DESCRIPTION);
    return $self;
}

my @axioms = ();
my @axioms_minus_keepers = ();

around 'execute' => sub {
    my $orig = shift;
    my $self = shift;
    my @arguments = @_;

    GetOptionsFromArray (
	\@arguments,
	'show-output' => \$opt_show_output,
	'show-premises' => \$opt_show_premises,
	'man' => \$opt_man,
	'verbose' => \$opt_verbose,
	'debug' => \$opt_debug,
	'help|?' => \$opt_help,
	'solution-szs-status=s' => \$opt_solution_szs_status,
	'with-prover=s' => \@opt_provers,
	'keep=s' => \@opt_keep,
	'timeout=i' => \$opt_timeout,
	'min-subset-size=i' => \$opt_min_subset_size,
	'max-subset-size=i' => \$opt_max_subset_size,
    ) or pod2usage (
	-exitval => 2,
	-input => pod_where({-inc => 1}, __PACKAGE__));

    if ($opt_help) {
        pod2usage(
	    -exitval => 1,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if ($opt_man) {
        pod2usage(
            -exitstatus => 0,
            -verbose    => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
        );
    }

    # debug implies verbose
    if ($opt_debug) {
        $opt_verbose = 1;
    }

    if ($opt_timeout < 0) {
	pod2usage (
	    -msg => error_message ($opt_timeout, $SPACE, 'is an inappropriate value for the prover timeout.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if ($opt_solution_szs_status eq $EMPTY_STRING) {
	pod2usage (
	    -msg => error_message ('The empty string is not an acceptable SZS problem status.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__));
    }

    if ($opt_solution_szs_status !~ /\A [A-Za-z]+ \z/) {
	pod2usage (
	    -msg => error_message ('Unacceptable SZS problem status', "\N{LF}", "\N{LF}", $TWO_SPACES, $opt_solution_szs_status),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if (scalar @opt_provers == 0) {
	@opt_provers = ('eprover', 'paradox');
    }

    my %provers = ();
    my @provers_no_dups = ();
    foreach my $tool (@opt_provers) {
	if (defined $provers{$tool}) {

	} else {
	    $provers{$tool} = 0;
	    push (@provers_no_dups, $tool);
	}
    }

    @opt_provers = @provers_no_dups;

    foreach my $prover (@opt_provers) {
	if (! known_prover ($prover)) {
	    my @supported_provers = supported_provers ();
	    say {*STDERR} error_message ('Unknown prover', $SPACE, $prover), $LF;
	    say {*STDERR} 'The following provers are known:', "\N{LF}";
	    if (scalar @supported_provers == 0) {
		say {*STDERR} '(none)';
	    } else {
		say asterisk_list (@supported_provers);
	    }
	    exit 1;
	}
    }

    if (scalar @arguments == 0) {
	pod2usage (
	    -msg => error_message ('Please supply a TPTP theory file.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if ($opt_min_subset_size < 0) {
	pod2usage (
	    -msg => error_message ($opt_min_subset_size, $SPACE, 'is not an appropriate value for the --min-subset-size option.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__));
    }

    if (defined $opt_max_subset_size && $opt_max_subset_size < 0) {
	pod2usage  (
	    -msg => error_message ($opt_min_subset_size, $SPACE, 'is not an appropriate value for the --max-subset-size option.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	    );
    }

    if (defined $opt_max_subset_size && $opt_max_subset_size < $opt_min_subset_size) {
	pod2usage (
	    -msg => error_message ('The maximum subset size is less than the minimum subset size.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if (scalar @arguments > 1) {
	pod2usage (
	    -msg => error_message ('Unable to make sense of the prove arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if (! ensure_tptp4x_available ()) {
	say STDERR error_message ('Cannot run tptp4X');
	exit 1;
    }

    if ($opt_solution_szs_status eq $EMPTY_STRING) {
	say {*STDERR} error_message ('The empty string is not an acceptable value for the intended SZS status.');
	exit 1;
    }

    if (! known_szs_status ($opt_solution_szs_status)) {
	say {*STDERR} error_message ('\'', $opt_solution_szs_status, '\' is not a known SZS status.');
	exit 1;
    }

    my $theory_path = $arguments[0];

    if (! ensure_readable_file ($theory_path)) {
	say {*STDERR} error_message ('The file at', $SPACE, $theory_path, $SPACE, 'does not exist (or is unreadable).');
	exit 1;
    }

    if (! ensure_sensible_tptp_theory ($theory_path)) {
	my $errors = tptp4X_output ($theory_path);
	say {*STDERR} error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	say {*STDERR} 'Here is what tptp4X gave as output when processing the file:';
	say {*STDERR} $errors;
	exit 1;
    }

    my $theory = Theory->new (path => $theory_path);
    my @axioms_by_name = $theory->get_axioms_by_name (1);

    @axioms = sort @axioms_by_name;

    if (scalar @axioms < $opt_min_subset_size) {
	say {*STDERR} error_message ('There are fewer axioms (', scalar @axioms, ') than were specified in the --min-subset-subset option.');
	exit 1;
    }

    foreach my $to_keep (@opt_keep) {
	if (none { $_ eq $to_keep } @axioms_by_name) {
	    say {*STDERR} error_message ($to_keep, $SPACE, 'is not the name of any axiom of ', $theory_path, $FULL_STOP);
	    exit 1;
	}
    }

    # Remove duplicates
    my %to_keep = ();
    foreach my $formula (@opt_keep) {
	$to_keep{$formula} = 0;
    }
    @opt_keep = keys %to_keep;

    $num_to_keep = scalar @opt_keep;

    if (defined $opt_max_subset_size && scalar @opt_keep > $opt_max_subset_size) {
	pod2usage (
	    -msg => error_message ('We were asked to keep', $SPACE, $num_to_keep, $SPACE, 'formulas, but this exceeds the value of the --max-subset-size option.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    my %axioms_minus_keepers = ();
    foreach my $axiom (@axioms) {
	if (! defined $to_keep{$axiom}) {
	    $axioms_minus_keepers{$axiom} = 0;
	}
    }

    @axioms_minus_keepers = keys %axioms_minus_keepers;

    return $self->$orig (@arguments);

};

sub success_judgment {
    my @szs_stauses = @_;
    my $index = first_index { is_szs_success ($_) } @szs_stauses;
    return ($index < 0 ? $SZS_UNKNOWN : $szs_stauses[$index]);
}

sub solve_greedily {
    my $theory = shift;
    my $tools_ref = shift;

    my %status = ();
    foreach my $tool (@opt_provers) {
	$status{$tool} = $SZS_NOT_TRIED;
    }

    foreach my $tool (@opt_provers) {
	my $szs_status = $theory->solve ($tool,
					 $opt_solution_szs_status,
					 { 'timeout' => $opt_timeout,
					   'debug' => $opt_debug });
	$status{$tool} = $szs_status;
	if (is_szs_success ($szs_status)) {
	    last;
	}
    }

    return \%status;

}

sub random_subset_containing_keepers {

    # Wanted: a random subset of @axioms that
    #
    # * contains all @opt_keep
    # * has at least $opt_min_subset_size elememts
    # * has at most $opt_max_subset_size elements, if that is defined

    my @answer = @opt_keep;

    my @shuffled = shuffle @axioms_minus_keepers;

    foreach my $i (1 .. ($opt_min_subset_size - scalar @opt_keep)) {
	my $elt = pop @shuffled;
	push (@answer, $elt);
    }

    # The set @answer includes @opt_keep and some random elements that
    # ensure that it has at least $opt_min_subset_size elements.

    if (defined $opt_max_subset_size) {
	foreach my $i (scalar @answer + 1 .. $opt_max_subset_size) {
	    if (int rand 2) {
		my $elt = pop @shuffled;
		push (@answer, $elt);
	    }
	}
    } else {
	while (defined (my $elt = shift @shuffled)) {
	    if (int rand 2) {
		push (@answer, $elt);
	    }
	}
    }

    if (wantarray) {
	return @answer;
    } else {
	return \@answer;
    }

}

sub factorial {
    my $n = shift;
    if ($n < 0) {
	return 0;
    } elsif ($n == 0) {
	return 1;
    } else {
	my $x = 1;
	foreach my $i (1 .. $n) {
	    $x = $x * $i;
	}
	return $x;
    }
}

memoize ('n_choose_k');
sub n_choose_k {
    my $n = shift;
    my $k = shift;
    if ($k > $n) {
	return 0;
    } else {
	return factorial ($n) / (factorial ($k) * factorial ($n - $k));
    }
}

my %encountered_subsets = ();

memoize ('num_combinations_from_to');
sub num_combinations_from_to {
    my $from = shift;
    my $to = shift;
    my $x = 0;
    foreach my $i ($from .. $to) {
	$x += n_choose_k (scalar @axioms_minus_keepers, $i);
    }
    return $x;
}

sub next_random_subset {

    my $max_subset_size
	= defined $opt_max_subset_size ? $opt_max_subset_size : scalar @axioms;

    # carp 'Max subset size: ', $max_subset_size;

    my $num_subsets = num_combinations_from_to ($opt_min_subset_size,
						$max_subset_size);

    # carp 'Num subsets: ', $num_subsets;

    if (scalar keys %encountered_subsets < $num_subsets) {

	# carp 'So far we have seen ', scalar keys %encountered_subsets, ' subsets';

	my @set = random_subset_containing_keepers ();

	@set = sort @set;
	my $set_as_string = scalar @set == 0 ? $EMPTY_STRING : join ($COMMA, @set);

	while (defined $encountered_subsets{$set_as_string}) {
	    # carp 'We have already seen ', $set_as_string, ' before';
	    @set = random_subset_containing_keepers ();
	    @set = sort @set;
	    $set_as_string = scalar @set == 0 ? $EMPTY_STRING : join ($COMMA, @set);
	}

	# carp 'Escape!';

	$encountered_subsets{$set_as_string} = 0;

	if (wantarray) {
	    return @set;
	} else {
	    return \@set;
	}
    } else {
	return undef;
    }

}

sub summarize_single_result {
    my $tool = shift;
    my $szs_status = shift;
    return "* ${tool} ==> ${szs_status}";
}

sub summarize_trial {
    my %prover_status = %{shift ()};

    my $summary = $EMPTY_STRING;

    my @summaries
	= map { summarize_single_result ($_, $prover_status{$_}) }
	    @opt_provers;

    my $prover_summary = join ($LF, @summaries);

    return $prover_summary;

}

sub candidate_is_known {
    my $candidate_ref = shift;
    my $losing_candidates_ref = shift;

    my @candidate = @{$candidate_ref};
    my @losing_candidates = @{$losing_candidates_ref};

    return any { sublist (\@candidate, $_) } @losing_candidates;

}

my $one_unknown_candidate = 0;

sub execute {
    my $self = shift;
    my @arguments = @_;

    my @losers = ();

    my $theory_path = $arguments[0];
    my $theory = Theory->new (path => $theory_path);

    my $num_trials = 0;
    my $subset_ref = next_random_subset ();

    while (defined $subset_ref) {

	$num_trials++;
	my @subset = @{$subset_ref};
	print 'Trial', $SPACE, $num_trials, $SPACE, '(', scalar @subset, $SPACE, 'axioms)', $SPACE;

	# carp 'Subset:', Dumper (@subset);

	$subset_ref = next_random_subset ();

	my %status = ();

	foreach my $prover (@opt_provers) {
	    $status{$prover} = $SZS_NOT_TRIED;
	}

	if (candidate_is_known (\@subset, \@losers)) {
	    say colored ($STRONGLY_UNSUCCESSFUL, $BAD_COLOR), $SPACE, '(a known non-solution includes this candidate)';
	    say summarize_trial (\%status);
	    next;
	}

	my $restricted = $theory->restrict_to_by_name (@subset);

	%status = %{solve_greedily ($restricted)};

	my @judgments = values %status;
	my $overall_judgment = success_judgment (@judgments);

	if (is_szs_success ($overall_judgment)) {
	    if (szs_implies ($overall_judgment, $opt_solution_szs_status)) {
		say colored ($SUCCESS, $GOOD_COLOR);
		say summarize_trial (\%status);
		print_formula_names_with_color (\@subset, $USED_PREMISE_COLOR);

		exit 0; # <--- bail out hard -- we won!

	    } elsif (szs_contradicts ($overall_judgment, $opt_solution_szs_status)) {
		push (@losers, \@subset);
		say colored ($STRONGLY_UNSUCCESSFUL, $BAD_COLOR), $SPACE, '(any later candidate that is included in this one will be ignored)';
	    } else {
		say colored ('Um...', $UNKNOWN_COLOR), $SPACE, '(', $overall_judgment, ')';
		say colored ($WEAKLY_UNSUCCESSFUL, $UNKNOWN_COLOR);
		$one_unknown_candidate = 1;
	    }
	} else {
	    say colored ($WEAKLY_UNSUCCESSFUL, $UNKNOWN_COLOR), $SPACE, '(unable to reach a decision)';
	    $one_unknown_candidate = 1;
	}

	say summarize_trial (\%status);

    }

    say $LF, 'We have exhausted all possible subsets.';

    if ($one_unknown_candidate) {
	say 'The best we can say is that the answer is', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $FULL_STOP;
    } else {
	say 'We found no candidate on which we could not render judgment,';
	say 'yet we found no candidate that solves the problem.';
	if (defined $opt_max_subset_size) {
	    if ($opt_max_subset_size < scalar @axioms) {
		say 'Since a nontrivial upper bound was placed on the subsets we considered,';
		say 'it would seem that the the best we can say is that the answer is', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $FULL_STOP;
	    } elsif ($opt_min_subset_size > 0) {
		say 'Since a nontrivial lower bound was placed on the subsets we considered,';
		say 'it would seem that the the best we can say is that the answer is', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $FULL_STOP;
	    } else {
		say 'Since no bounds were placed on the sizes of the subsets that we considered,';
		say 'it would seem that', $SPACE, colored ('no', $BAD_COLOR), $SPACE, 'candidate solves the problem as we wanted';
		say '(For no subset can we contradict the intended SZS status', $SPACE, $opt_solution_szs_status, '.)';
	    }
	} elsif ($opt_min_subset_size > 0) {
	    say 'Since a nontrivial lower bound was placed on the subsets we considered,';
	    say 'it would seem that the the best we can say is that the answer is', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $FULL_STOP;
	} else {
	    say 'Since no bounds were placed on the sizes of the subsets that we considered,';
	    say 'it would seem that', $SPACE, colored ('no', $BAD_COLOR), $SPACE, 'solves the problem as we wanted';
	    say '(For no subset can we contradict the intended SZS status', $SPACE, $opt_solution_szs_status, '.)';
	}
    }

}

1;
__END__

=pod

=head1 NAME

tipi vegas

=head1 SYNOPSIS

tipi vegas [ --help | --man ]

tipi vegas [ --verbose | --debug] [ --solution-szs-status=STATUS ] [ --with-prover=PROVER] [ --keep=FORMULA-NAME ] [ --timeout=N ] [ --min-subset-size=N ] [ --max-subset-size=N ] TPTP-file

=head1 DESCRIPTION

B<tipi vegas> is simply a Las Vegas algorithm for solving a TPTP
problem.  We simply enumerate random combinations of premises from the
given TPTP problem and try to solve the problem using those premises.
If we can solve the problem, we win.  Otherwise, we just keep going.

If the C<--timeout> option is absent, a default timeout of 30 seconds
will be used.

The theorem prover specified in the C<--with-prover> option will be used.
One can repeat this option.  The interpretation is that you are
specifying a set of theorem provers to be used to determine
(un)derivability.  If you omit specifying this option, then by
default, two provers will be used: E and Paradox.  If the
C<--with-prover> option is used, these defaults will be discarded, and
all and only the provers you specify will be used.

The C<--solution-szs-status> option is used to indicate what it means
to solve the TPTP problem.  The default is B<Theorem>, i.e., the
problem is successfuly solved if a theorem prover assigns the SZS
status B<Theorem> to the problem (or some other SZS status that
implies B<Theorem>).

Use C<--keep> to ensure that we do not consider subsets that lack a
certain formula.  This option can be repeated to specify that multiple
formulas should be kept.

By default, the minimum subset size we consider is 0, and the maximum
is the cardinality of the set of premises of the given TPTP file.  Use
C<--min-subset-size> and C<--max-subset-size> to constrain the
possibilities.

=head1 SEE ALSO

=over 8

=item L<Las Vegas algorithms (Wikipedia)|http://en.wikipedia.org/wiki/Las_vegas_algorithm>

=back

=cut
