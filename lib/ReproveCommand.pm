package ReproveCommand;

require v5.10;

use Moose;
use Carp qw(croak carp cluck);
use Pod::Find qw(pod_where);
use Pod::Usage;
use Readonly;
use Getopt::Long qw(GetOptionsFromArray :config gnu_compat);
use charnames qw(:full);
use English qw(-no_match_vars);
use Data::Dumper;
use Term::ANSIColor qw(colored);
use feature 'say';
use List::Util qw(max);
use List::MoreUtils qw(any);
use Term::ProgressBar;
use Regexp::DefaultFlags;
use POSIX qw(ceil);
use Algorithm::Combinatorics qw(combinations);
use IPC::Cmd qw(can_run);

extends 'Command';

use Theory;
use TPTP qw(ensure_tptp4x_available
	    ensure_valid_tptp_file
	    prove_if_possible
	    ensure_sensible_tptp_theory
	    tptp4X_output
	    );
use Utils qw(error_message
	     all_sublists
	     all_nonempty_sublists
	     remove_duplicate_lists
	     subtuple
	     tuple_less_than_wrt_ordering
	     ensure_readable_file);
use SZS qw(szs_implies);

Readonly my $EMPTY_STRING => q{};
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $COMMA => q{,};
Readonly my $COLON => q{:};
Readonly my $SLASH => q{/};
Readonly my $FULL_STOP => q{.};
Readonly my $LF => "\N{LF}";
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'yellow';
Readonly my $DESCRIPTION => 'Prove a conjecture again, focusing in on needed premises.';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';
Readonly my $NEEDED_PREMISE_COLOR => 'red';
Readonly my $UNNEEDED_PREMISE_COLOR => 'cyan';

my $opt_show_output = 0;
my $opt_help = 0;
my $opt_man = 0;
my $opt_verbose = 0;
my $opt_debug = 0;
my $opt_solution_szs_status = 'Theorem';
my $opt_proof_finder = 'eprover';
my $opt_model_finder = 'paradox';
my $opt_syntactically = 0;
my $opt_semantically = 0;
my $opt_model_finder_timeout = 5;
my $opt_proof_finder_timeout = 30;
my $opt_skip_initial_proof = 0;
my $opt_show_only_final_used_premises = 0;
my $opt_show_only_final_unused_premises = 0;

sub BUILD {
    my $self = shift;
    $self->_set_description ($DESCRIPTION);
    return $self;
}

sub fill_up_to_column {
    my $str = shift;
    my $column = shift;

    my $str_len = length $str;
    my $padding = $str_len < $column ? $SPACE x ($column - $str_len) : $EMPTY_STRING;
    return ($str . $padding);

}

sub print_formula_names_with_color {
    my $formulas_ref = shift;
    my $color = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my @formulas = @{$formulas_ref};

    if (scalar @formulas > 0) {

	if (defined $parameters{'sorted'} && $parameters{'sorted'}) {
	    my @formula_names_sorted = sort @formulas;
	    my @formula_names_colored
		= map { colored ($_, $color) } @formula_names_sorted;
	    say join ("\N{LF}", @formula_names_colored);
	} else {
	    my @formula_names_colored = map { colored ($_, $color) } @formulas;
	    say join ("\N{LF}", @formula_names_colored);
	}

    }

    return 1;

}

around 'execute' => sub {
    my $orig = shift;
    my $self = shift;
    my @arguments = @_;

    GetOptionsFromArray (
	\@arguments,
	'man' => \$opt_man,
	'verbose' => \$opt_verbose,
	'help|?' => \$opt_help,
	'debug' => \$opt_debug,
	'semantically' => \$opt_semantically,
	'syntactically' => \$opt_syntactically,
	'solution-szs-status=s' => \$opt_solution_szs_status,
	'proof-finder=s' => \$opt_proof_finder,
	'model-finder=s' => \$opt_model_finder,
	'model-finder-timeout=i' => \$opt_model_finder_timeout,
	'proof-finder-timeout=i' => \$opt_proof_finder_timeout,
	'only-used' => \$opt_show_only_final_used_premises,
	'only-unused' => \$opt_show_only_final_unused_premises,
    ) or pod2usage (
	-exitval => 2,
	-input => pod_where({-inc => 1}, __PACKAGE__),
    );

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

    if (scalar @arguments == 0) {
	pod2usage (
	    -msg => error_message ('Please supply a TPTP theory file.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if (scalar @arguments > 1) {
	pod2usage (
	    -msg => error_message ('Unable to make sense of the arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @arguments)),
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

    if ($opt_syntactically && $opt_semantically) {
	pod2usage (
	    -msg => error_message ('Please choose which reprove procedure you would like to use (the two options are --syntactically and --semantically).'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    # debult to syntactically
    if (! $opt_semantically) {
	$opt_syntactically = 1;
    }

    if ($opt_model_finder_timeout < 0) {
	pod2usage (
	    -msg => error_message ('Invalid value ', $opt_model_finder_timeout, ' for the model-finder timeout option.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if ($opt_proof_finder_timeout < 0) {
	pod2usage (
	    -msg => error_message ('Invalid value ', $opt_proof_finder_timeout, ' for the proof-finder timeout option.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if (! ensure_tptp4x_available ()) {
	say STDERR error_message ('Cannot run tptp4X');
	exit 1;
    }

    if ($opt_proof_finder eq $EMPTY_STRING) {
	pod2usage (
	    -msg => error_message ('The empty string is not an acceptable value for the --proof-finder option.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if (! can_run ($opt_proof_finder)) {
	say {*STDERR} error_message ('Cannot run', $SPACE, $opt_proof_finder, $FULL_STOP);
	exit 1;
    }

    if ($opt_model_finder eq $EMPTY_STRING) {
	pod2usage (
	    -msg => error_message ('The empty string is not an acceptable value for the --proof-finder option.'),
	    -exitval => 2,
	    -input => pod_where({-inc => 1}, __PACKAGE__),
	);
    }

    if (! can_run ($opt_model_finder)) {
	say {*STDERR} error_message ('Cannot run', $SPACE, $opt_model_finder, $FULL_STOP);
	exit 1;
    }

    my $theory_path = $arguments[0];

    if (! ensure_readable_file ($theory_path)) {
	say STDERR error_message ('There is no file at', $SPACE, $theory_path, $SPACE, '(or it is unreadable).');
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

    if ($theory->is_first_order ()) {
	return $self->$orig (@arguments);
    } else {
	say {*STDERR} error_message ('The theory at', $SPACE, $theory_path, $SPACE, 'seems to be a non-first-order theory.');
    }

};

sub execute {
    my $self = shift;
    my @arguments = @_;

    my $theory_path = $arguments[0];
    my $theory = Theory->new (path => $theory_path);

    if ($opt_syntactically) {
	return reprove_syntactically ($theory);
    } elsif ($opt_semantically) {
	return reprove_semantically ($theory);
    } else {
	say STDERR error_message ('We need to know which reprove method should be use; somehow, neither of the two methods was selected.');
	exit 1;
    }

}

sub prove {
    my $theory = shift;
    return TPTP::prove ($theory,
			$opt_proof_finder,
			{ 'timeout' => $opt_proof_finder_timeout });
}

sub reprove_syntactically {
    my $theory = shift;
    my @used_premises = ();
    my @unused_premises = $theory->get_formulas_by_name (1);
    my $result = prove ($theory);
    my $derivation = $result->output_as_derivation ();
    my $szs_status = $result->get_szs_status ();

    print 'PREMISES (', colored ('used', $USED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unused', $UNUSED_PREMISE_COLOR), ')', "\N{LF}";

    while (defined $derivation
	       && szs_implies ($szs_status, $opt_solution_szs_status)
		   && scalar @unused_premises > 0) {

	$theory = $derivation->theory_from_used_premises ();
	$result = prove ($theory);
	$szs_status = $result->get_szs_status ();
	$derivation = $result->output_as_derivation ();
	@used_premises = $derivation->get_used_premises ();
	@unused_premises = $derivation->get_unused_premises ();

	if (! $opt_show_only_final_used_premises) {
	    print_formula_names_with_color (\@unused_premises, $UNUSED_PREMISE_COLOR);
	}

    }

    if (defined $derivation) {
	if (szs_implies ($szs_status, $opt_solution_szs_status)) {
	    if ($opt_show_only_final_used_premises) {
		print_formula_names_with_color (\@used_premises);
	    } elsif ($opt_show_only_final_unused_premises) {
		# nothing to do
	    } else {
		print_formula_names_with_color (\@used_premises, $USED_PREMISE_COLOR);
	    }
	} else {
	    say 'We obtained the SZS status', $SPACE, $szs_status, ', which does not imply the intended SZS status (', $opt_solution_szs_status, ').';
	}
    } else {
	say 'We failed to interpret the result of a call to', $SPACE, $opt_proof_finder_timeout, $SPACE, 'as a derivation.';
	say 'We cannot, then, extract the set of used premises.';
    }

    return 1;

}

sub reprove_semantically {
    my $theory = shift;

    my $theory_path = $theory->get_path ();

    my @axioms = $theory->get_axioms (1);

    my $theory_has_conjecture = $theory->has_conjecture_formula ();
    my $conjecture = undef;
    if ($theory->has_conjecture_formula ()) {
	$conjecture = $theory->get_conjecture ();
    }

    if ($theory->has_conjecture_formula ()) {
	$theory = $theory->promote_conjecture_to_false_axiom ();
    }

    my %needed = ();
    my %unneeded = ();
    my %unknown = ();

    print 'PREMISES (', colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, $SLASH, $SPACE, colored ('unknown', $UNKNOWN_COLOR), ')', "\N{LF}";

    my @sorted_axioms = sort { $a->get_name() cmp $b->get_name () } @axioms;

    foreach my $axiom (@sorted_axioms) {
	my $axiom_name = $axiom->get_name ();
	my $trimmed_theory = $theory->remove_formula ($axiom);
	my $satisfiable = $trimmed_theory->is_satisfiable ({'timeout' => $opt_model_finder_timeout});

	if ($satisfiable == -1) {
	    say colored ($axiom_name, $UNKNOWN_COLOR);
	    $unknown{$axiom_name} = 0;
	} elsif ($satisfiable == 0) {
	    say colored ($axiom_name, $UNNEEDED_PREMISE_COLOR);
	    $unneeded{$axiom_name} = 0;
	} else {
	    say colored ($axiom_name, $NEEDED_PREMISE_COLOR);
	    $needed{$axiom_name} = 0;
	}

    }

    if (defined $conjecture) {
	print 'Now try deriving the conjecture from only the', $SPACE, scalar keys %needed, $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premise(s):';
    } else {
	print 'Now try solving the problem from only the', $SPACE, scalar keys %needed, $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), ' premise(s):';
    }

    # Dump everything that is not known to be needed
    my $small_theory = $theory->copy ();
    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	if (defined $needed{$axiom_name}) {
	    if ($opt_debug) {
		say {*STDERR} $axiom_name, $SPACE, 'is needed, so we are keeping it.';
	    }
	} else {
	    $small_theory = $small_theory->remove_formula ($axiom);
	    if ($opt_debug) {
		say {*STDERR} $axiom_name, $SPACE, 'is not needed, so it has been removed.';
	    }
	}
    }

    # Remove the old conjecture, which was promoted to a false axiom,
    # and put it back as the conjecture.
    if (defined $conjecture) {
	$small_theory = $small_theory->strip_conjecture ($conjecture);
	$small_theory = $small_theory->add_formula ($conjecture);
    }

    my $new_result = TPTP::prove ($small_theory,
				  $opt_proof_finder,
				  { 'timeout' => $opt_proof_finder_timeout });
    my $new_result_szs_status
	= $new_result->has_szs_status () ? $new_result->get_szs_status () : 'Unknown';

    if ($new_result_szs_status eq $opt_solution_szs_status) {
	say $SPACE, colored ('OK', $GOOD_COLOR);
    } else {
	say $SPACE, colored ('Not OK', $BAD_COLOR), ' (SZS status ', $new_result_szs_status, ')';
    }

    if ($new_result_szs_status eq $opt_solution_szs_status) {
	if (defined $conjecture) {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone suffice to prove the conjecture.';
	} else {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone suffice to solve the problem.';
	}
    } else {
	if (defined $conjecture) {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone do not suffice to prove the conjecture.';
	} else {
	    say 'The', $SPACE, colored ('needed', $NEEDED_PREMISE_COLOR), $SPACE, 'formulas alone do not suffice to solve the problem.';
	}
	say 'Consider the minimize command to search for combinations of', $SPACE, colored ('unknown', $UNKNOWN_COLOR), $SPACE, 'and', $SPACE, colored ('unneeded', $UNNEEDED_PREMISE_COLOR), $SPACE, 'formulas';
	say 'that suffice to solve the problem.';

    }

    return 1;

}

1;
__END__

=pod

=head1 NAME

tipi reprove

=head1 SYNOPSIS

tipi reprove --help

tipi reprove --man

tipi reprove [ --verbose | --debug ] [ --syntactically | --semantically ] [--proof-finder=PROVER] [--model-finder=MODEL-FINDER] [--proof-finder-timeout=N] [--model-finder-timeout=N] [--solution-szs-status=STATUS] TPTP-file

=head1 DESCRIPTION

B<tipi reprove> attempts to solve a problem from fewer premises.

There are two modes in which B<tipi reprove> operates: syntactically
or semantically (indicated by the C<--syntactically> and
C<--semantically> options).  One or the other has to be chosen.  By
default, the syntactic reprove method is chosen.  We describe the two
methods in detail below; we now proceed to descrbe options common to
both.

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

=head2 Syntactic reproving

The definition of syntactic reproving is simple: from a solution of a
problem, extract the used and unused premises.  If there are unused
premises, throw these away from the problem and try again.  Keep doing
this until we find that there are no unused premises, i.e., all
premises are used.

=head2 Semantic reproving

Semantic reproving proceeds by simply deleting premises that are known
to be unnecessary.  Generally one uses semantic methods (such as using
finite model finders) to determine underivability, which is a sign
that a premise really is needed (if it is deleted, then the problem
becomes countersatisfiable).  Hence the name "semantic reproving".

We work only with SZS statuses.  Thus if the intended SZS status of
your problem is not B<Theorem>, use the C<--solution-szs-status>
option to specify it.  We will then try, for each premise, to see
whether we can reach a solution SZS status that implies the negation
of the SZS status you provided.

=head1 SEE ALSO

=over 8

=item L<The SZS Ontology|http://www.cs.miami.edu/~tptp/cgi-bin/SeeTPTP?Category=Documents&File=SZSOntology>

=back

=cut
