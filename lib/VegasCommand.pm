package VegasCommand;

require v5.10;

use Moose;
use Carp qw(croak carp);
use Pod::Usage;
use Readonly;
use Getopt::Long qw(GetOptionsFromArray :config gnu_compat);
use charnames qw(:full);
use English qw(-no_match_vars);
use Data::Dumper;
use Term::ANSIColor qw(colored);
use Regexp::DefaultFlags;
use List::Util qw(shuffle);
use List::MoreUtils qw(any none first_value first_index);
use feature 'say';

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    known_prover);
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

my $opt_show_output = 0;
my $opt_show_premises = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_solution_szs_status = 'Theorem';
my @opt_provers = ();
my @opt_keep = ();
my $opt_timeout = 30; # seconds
my $opt_increasing = 0;
my $opt_decreasing = 0;

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
	my @formula_names_colored = map { $_->name_with_color ($color) } @formulas;
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
	'increasing' => \$opt_increasing,
	'decreasing' => \$opt_decreasing,
    ) or pod2usage (2);

    if ($opt_help) {
        pod2usage(1);
    }

    if ($opt_man) {
        pod2usage(
            -exitstatus => 0,
            -verbose    => 2
        );
    }

    # debug implies verbose
    if ($opt_debug) {
        $opt_verbose = 1;
    }

    if ($opt_timeout < 0) {
	pod2usage (-msg => error_message ($opt_timeout, $SPACE, 'is an inappropriate value for the prover timeout.'),
		   -exitval => 2);
    }

    if ($opt_solution_szs_status eq $EMPTY_STRING) {
	pod2usage (-msg => error_message ('The empty string is not an acceptable SZS problem status.'),
		   -exitval => 2);
    }

    if ($opt_solution_szs_status !~ /\A [A-Za-z]+ \z/) {
	pod2usage (-msg => error_message ('Unacceptable SZS problem status', "\N{LF}", "\N{LF}", $TWO_SPACES, $opt_solution_szs_status),
		   -exitval => 2);
    }

    if (scalar @opt_provers == 0) {
	@opt_provers = ('eprover', 'paradox');
    }

    my %provers = ();

    foreach my $tool (@opt_provers) {
	$provers{$tool} = 0;
    }

    @opt_provers = keys %provers;

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
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2);
    }

    if (scalar @arguments > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the prove arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
		   -exitval => 2);
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
	say STDERR error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	exit 1;
    }

    my $theory = Theory->new (path => $theory_path);
    my @axioms_by_name = $theory->get_axioms_by_name (1);

    @axioms = sort @axioms_by_name;

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

sub random_subset {
    my @set = @_;

    my $num_elements = scalar @set;
    my $size = rand $num_elements;

    my @shuffled = shuffle @set;
    my @next = @shuffled[0 .. $size - 1];

    if (wantarray) {
	return @next;
    } else {
	return \@next;
    }

}

sub random_subset_containing_keepers {
    my @set = random_subset (@axioms);

    foreach my $to_keep (@opt_keep) {
	if (! list_member ($to_keep, \@set)) {
	    push (@set, $to_keep);
	}
    }

    if (wantarray) {
	return @set;
    } else {
	return \@set;
    }

}

my %encountered_subsets = ();

sub next_random_subset {
    my @set = random_subset_containing_keepers ();

    @set = sort @set;
    my $set_as_string = join ($COMMA, @set);

    while (defined $encountered_subsets{$set_as_string}) {
	@set = random_subset_containing_keepers ();
	@set = sort @set;
	$set_as_string = join ($COMMA, @set);
    }

    $encountered_subsets{$set_as_string} = 0;

    if (wantarray) {
	return @set;
    } else {
	return \@set;
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

sub execute {
    my $self = shift;
    my @arguments = @_;

    my @losers = ();

    my $theory_path = $arguments[0];
    my $theory = Theory->new (path => $theory_path);
    my @axioms = $theory->get_axioms (1);

    my $num_trials = 0;
    my $subset_ref = next_random_subset ();

    while (defined $subset_ref) {

	$num_trials++;
	print 'Trial', $SPACE, $num_trials, $SPACE;
	my @subset = @{$subset_ref};

	$subset_ref = next_random_subset ();

	my %status = ();

	foreach my $prover (@opt_provers) {
	    $status{$prover} = $SZS_NOT_TRIED;
	}

	if (any { sublist ($_, \@subset) } @losers) {
	    say colored ($WEAKLY_SUCCESSFUL, $UNKNOWN_COLOR), $SPACE, '(a known non-solution extends this candidate)';
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

		exit 0; # <--- the only exit from this infinite loop

	    } elsif (szs_contradicts ($overall_judgment, $opt_solution_szs_status)) {
		# say colored ('AWESOME (syntactically)', 'red');
		push (@losers, \@subset);
		say colored ($STRONGLY_UNSUCCESSFUL, $BAD_COLOR), $SPACE, '(any later candidate that includes this one will be ignored)';
	    } else {
		say colored ('Um...', $UNKNOWN_COLOR);
		say colored ($WEAKLY_UNSUCCESSFUL, $UNKNOWN_COLOR);
	    }
	} else {
	    say colored ($WEAKLY_UNSUCCESSFUL, $UNKNOWN_COLOR), $SPACE, '(unable to reach a decision)';
	}

	say summarize_trial (\%status);

    }

    say 'It seems we have exhausted all possible subsets.';

}

1;
__END__
