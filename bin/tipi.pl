#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long qw(:config gnu_compat pass_through);
use Pod::Usage;
use Readonly;
use charnames qw(:full);
use English qw(-no_match_vars);
use version;
use Carp qw(croak carp);
use Term::ANSIColor;
use Regexp::DefaultFlags;
use List::MoreUtils qw(all);
use Data::Dumper;

use FindBin qw($RealBin);
use lib "$RealBin/../lib";

use Theory;
use TPTP qw(ensure_tptp4x_available ensure_valid_tptp_file prove_if_possible);
use Utils qw(ensure_readable_file);

Readonly my $VERSION => qv('1.0');

# Constant strings
Readonly my $EMPTY_STRING => q{};
Readonly my $SPACE => q{ };
Readonly my $TWO_SPACES => q{  };

# Colors
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'bright_black';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $UNKNOWN_COLOR => 'yellow';

Readonly my @COMMANDS => (
    'prove',
    'reprove',
    'premises',
    'conjecture',
    'model',
    'reprove-semantically',
    # 'init',
    );

Readonly my %COMMAND_ROUTINES =>
    (
	'reprove' => \&cmd_reprove,
	'reprove-semantically' => \&cmd_reprove_semantically,
	'prove' => \&cmd_prove,
	'premises' => \&cmd_premises,
	'conjecture' => \&cmd_conjecture,
	'model' => \&cmd_model,
    );

Readonly my %COMMAND_DESCRIPTIONS =>
    (
	'prove'   => 'Try proving a conjecture.',
	'reprove' => 'Repeatedly reprove a conjecture using only actually used premises until no further trimming is possible.',
	'reprove-semantically' => 'Test whether the deletion of a premises leads to countersatisfiability.',
	'premises' => 'Print the (names of the) premises of a TPTP theory file.',
	'conjecture' => 'Print the name of the conjecture (if present) in a TPTP theory file.',
	'model' => 'Find a model for a TPTP theory.',
    );

my $opt_man       = 0;
my $opt_help      = 0;
my $opt_verbose   = 0;
my $opt_debug     = 0;
my $opt_version   = 0;

sub describe_command {
    my $command_name = shift;
    if (defined $COMMAND_DESCRIPTIONS{$command_name}) {
	return $COMMAND_DESCRIPTIONS{$command_name};
    } else {
	print {*STDERR} message (error_message ('No description is known for the command \'', $command_name, '\'.'));
	exit 1;
    }
}

sub ensure_sensible_commands {

    my $all_descriptions_known = all { defined $COMMAND_DESCRIPTIONS{$_} } @COMMANDS;
    my $all_subroutines_defined = all { defined $COMMAND_ROUTINES{$_} } @COMMANDS;

    return ($all_descriptions_known && $all_subroutines_defined);

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

sub summarize_commands {

    my $summary = $EMPTY_STRING;

    foreach my $command (@COMMANDS) {
	my $description = describe_command ($command);
	$summary .= message ('  * ', colored ($command, 'blue'), ' -- ', $description);
    }

    return $summary;

}

sub process_commandline {

    GetOptions(
        'help|?'      => \$opt_help,
        'man'         => \$opt_man,
        'verbose'     => \$opt_verbose,
        'debug'       => \$opt_debug,
        'version'     => \$opt_version,
    ) or pod2usage(2);

    if ($opt_help) {
        pod2usage(1);
    }

    if ($opt_man) {
        pod2usage(
            -exitstatus => 0,
            -verbose    => 2
        );
    }

    if ($opt_version) {
        print message ($VERSION);
        exit 0;
    }

    # debug implies verbose
    if ($opt_debug) {
        $opt_verbose = 1;
    }

    if (scalar @ARGV == 0) {
	my $message = message_with_extra_padding ('Please supply a tipi command.  The available commands are:');
	$message .= message (summarize_commands ());
	$message .= message ('See the man page for more information.  (Invoke this program with the option \'--man\'.)');
	pod2usage (-message => error_message ($message),
		   -exitstatus => 2);
    }

    my $command = $ARGV[0];

    if (! defined $COMMAND_ROUTINES{$command}) {
	my $message = message_with_extra_linefeed ('Unknown tipi command');
	$message .= message_with_extra_linefeed ($TWO_SPACES, $command);
	$message .= message_with_extra_linefeed ('Supported commands:');
	$message .= message (summarize_commands ());
	$message .= message ('See the man page for more details.  (Invoke this program with the option \'--man\'.)');
	pod2usage (-message => error_message ($message),
		   -exitstatus => 2);

    }

    return 1;

}

if (! ensure_sensible_commands ()) {
    print {*STDERR} error_message ('Something is seriously wrong with the list of commands; please inform the maintainer.'), "\N{LF}";
    exit 1;
}

process_commandline ();

my $command_name = $ARGV[0];
shift @ARGV;

my $eval_command = eval { &{$COMMAND_ROUTINES{$command_name}} };
my $eval_message = $@;

if (defined $eval_command) {
    exit 0;
} elsif (defined $eval_message) {
    print {*STDERR} error_message ('Something went wrong executing the command \'', $command_name, '\'');
    if (scalar @ARGV == 0) {
	print {*STDERR} message (' (without any further arguments).');
    } else {
	my $argument_list = join ($SPACE, @ARGV);
	print {*STDERR} message (' with the arguments', "\N{LF}");
	print {*STDERR} message ($TWO_SPACES, $argument_list, "\N{LF}");
    }
    print {*STDERR} message ('The error was:', "\N{LF}");
    print {*STDERR} message ($eval_message);
    print {*STDERR} message ('Please inform the maintainers.');
    exit 1;
} else {
    print {*STDERR} error_message ('Something went badly wrong trying to execute the command \'', $command_name, '\'.');
    print {*STDERR} message ('Please inform the maintainers.');
    exit 1;
}

######################################################################
## Common utilities
######################################################################

sub print_formula_names_with_color {
    my $formulas_ref = shift;
    my $color = shift;

    my @formulas = @{$formulas_ref};

    if (scalar @formulas == 0) {
	print '(none)';
    } else {
	my @formula_names = map { $_->get_name () } @formulas;
	if (defined $color) {
	    my @formula_names_colored = map { colored ($_, $color) } @formula_names;
	    print join ("\N{LF}", @formula_names_colored);
	} else {
	    print join ("\N{LF}", @formula_names);
	}
    }

    print "\N{LF}";

    return 1;

}

sub ensure_sensible_tptp_theory {

    my $theory_path = shift;

    if (! ensure_readable_file ($theory_path)) {
	print {*STDERR} message (error_message ('No such file at ', $theory_path, ' (or the file is unreadable).'));
	exit 1;
    }

    if (! ensure_valid_tptp_file ($theory_path)) {
	print {*STDERR} message (error_message ($theory_path, ' is not a valid TPTP file.'));
	exit 1;
    }

}

sub report_used_and_unused_premises {
    my $derivation = shift;

    my @unused_premises = $derivation->get_unused_premises ();
    my @used_premises = $derivation->get_used_premises ();

    my @used_premise_names = map { $_->get_name () } @used_premises;
    my @unused_premise_names = map { $_->get_name () } @unused_premises;

    my %used_premises_table = ();
    foreach my $premise (@used_premise_names) {
	$used_premises_table{$premise} = 0;
    }

    my $background = $derivation->get_background_theory ();
    my @axioms = $background->get_axioms ();
    my @axiom_names = map { $_->get_name () } @axioms;
    my @sorted_axiom_names = sort @axiom_names;

    print 'PREMISES (', colored ('used', $USED_PREMISE_COLOR), ' / ', colored ('unused', $UNUSED_PREMISE_COLOR), ')', "\N{LF}";

    if (scalar @sorted_axiom_names == 0) {

	print '(none)', "\N{LF}";

    } else {

	foreach my $axiom (@sorted_axiom_names) {
	    if (defined $used_premises_table{$axiom}) {
		print colored ($axiom, $USED_PREMISE_COLOR);
	    } else {
		print colored ($axiom, $UNUSED_PREMISE_COLOR);
	    }
	    print "\N{LF}";
	}

    }

    return 1;

}

######################################################################
## Prove command
######################################################################

my $prove_opt_show_output = 0;
my $prove_opt_show_premises = 0;

sub prove_ensure_sensible_arguments {

    GetOptions (
	'show-output' => \$prove_opt_show_output,
	'show-premises' => \$prove_opt_show_premises,
    )
	or pod2usage (2);

    if (scalar @ARGV == 0) {
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2);
    }

    if (scalar @ARGV > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the prove arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @ARGV)),
		   -exitval => 2);
    }

    if (! ensure_tptp4x_available ()) {
	print {*STDERR} message (error_message ('Cannot run tptp4X'));
	exit 1;
    }

    my $theory_path = $ARGV[0];

    ensure_sensible_tptp_theory ($theory_path);

    return 1;

}

sub cmd_prove {

    prove_ensure_sensible_arguments ();

    my $theory_path = $ARGV[0];

    my $theory = Theory->new (path => $theory_path);

    my $tptp_result = eval { TPTP::prove ($theory) };
    my $tptp_message = $@;

    if (! defined $tptp_result) {
	if (defined $tptp_message) {
	    if ($tptp_message eq $EMPTY_STRING) {
		print {*STDERR} message (error_message ('Something went wrong proving ', $theory_path, ' (we received no further information).'));
		exit 1;
	    } else {
		print {*STDERR} message_with_extra_linefeed (error_message ('Something went wrong proving ', $theory_path, ':'));
		print {*STDERR} message ($tptp_message);
		exit 1;
	    }
	} else {
	    print {*STDERR} message (error_message ('Something went wrong proving ', $theory_path, ' (we received no further information).'));
	    exit 1;
	}
    }

    if (! $tptp_result->exited_cleanly ()) {
	print {*STDERR} message (error_message ('The prover terminated, but it did not exit cleanly when working with ', $theory_path, '.'));
	exit 1;
    }

    my $szs_status = eval { $tptp_result->get_szs_status () };

    if (defined $szs_status) {

	if ($szs_status eq 'Theorem') {
	    my $derivation = $tptp_result->output_as_derivation ();

	    if ($opt_debug) {
		print {*STDERR} 'The derivation was just obtained:', "\N{LF}", Dumper ($derivation);
	    }

	    print 'SZS Status: ', colored ($szs_status, $GOOD_COLOR), "\N{LF}";

	    if ($prove_opt_show_output) {
		print "\N{LF}", 'The prover output:', "\N{LF}";
		print $tptp_result->get_output ();
	    }

	    if ($prove_opt_show_premises) {
		print "\N{LF}";
		report_used_and_unused_premises ($derivation);
	    }

	} elsif ($szs_status eq 'CounterSatisfiable'
	     || $szs_status eq 'CounterTheorem') {

	    print 'SZS Status: ', colored ($szs_status, $BAD_COLOR), "\N{LF}";

	    if ($prove_opt_show_output) {
		print "\N{LF}", 'The prover output:', "\N{LF}";
		print $tptp_result->get_output ();
	    }

	}

    } else {

	print {*STDERR} 'Error: no SZS status for this prover run for ', $theory_path, '.', "\N{LF}";
	print {*STDERR} 'The prover output was:', "\N{LF}";
	print {*STDERR} $tptp_result->get_output ();
	exit 1;

    }

    return 1;

}

######################################################################
## Reprove command
######################################################################

sub reprove_ensure_sensible_arguments {

    if (scalar @ARGV == 0) {
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2);
    }

    if (scalar @ARGV > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the reprove arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @ARGV)),
		   -exitval => 2);
    }

    my $theory_path = $ARGV[0];

    if (! ensure_tptp4x_available ()) {
	print {*STDERR} message (error_message ('Cannot run tptp4X'));
	exit 1;
    }

    ensure_sensible_tptp_theory ($theory_path);

    return 1;

}

sub cmd_reprove {

    reprove_ensure_sensible_arguments ();

    my $theory_path = $ARGV[0];

    my $theory = Theory->new (path => $theory_path);

    my $derivation = prove_if_possible ($theory);

    if ($opt_debug) {
	print {*STDERR} 'The derivation was just obtained:', "\N{LF}", Dumper ($derivation);
    }

    print 'PREMISES (', colored ('used', $USED_PREMISE_COLOR), ' / ', colored ('unused', $UNUSED_PREMISE_COLOR), ')', "\N{LF}";

    my @unused_premises = $derivation->get_unused_premises ();

    while (scalar @unused_premises > 0) {

	print_formula_names_with_color (\@unused_premises, $UNUSED_PREMISE_COLOR);

	$theory = $derivation->theory_from_used_premises ();
	$derivation = prove_if_possible ($theory);
	@unused_premises = $derivation->get_unused_premises ();

    }

    my @used_premises = $derivation->get_used_premises ();
    print_formula_names_with_color (\@used_premises, $USED_PREMISE_COLOR);

    return 1;

}

######################################################################
## Premises
######################################################################

my $premises_opt_expand_includes = 1;

sub premises_ensure_sensible_arguments {

    GetOptions (
	'expand!' => \$premises_opt_expand_includes,
    )
	or pod2usage (2);

    if (scalar @ARGV == 0) {
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2);
    }

    if (scalar @ARGV > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the premises arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @ARGV)),
		   -exitval => 2);
    }

    if (! ensure_tptp4x_available ()) {
	print {*STDERR} message (error_message ('Cannot run tptp4X'));
	exit 1;
    }

    my $theory_path = $ARGV[0];

    ensure_sensible_tptp_theory ($theory_path);

    return 1;

}

sub cmd_premises {

    premises_ensure_sensible_arguments ();

    my $theory_path = $ARGV[0];

    my $theory = Theory->new (path => $theory_path);

    my @premises = $theory->get_axioms ($premises_opt_expand_includes);

    if (scalar @premises > 0) {
	print_formula_names_with_color (\@premises, undef);
    }

    return 1;

}

######################################################################
## Conjecture
######################################################################

my $conjecture_opt_name_only = 1;

sub conjecture_ensure_sensible_arguments {

    GetOptions (
	'name-only' => \$conjecture_opt_name_only,
    )
	or pod2usage (2);

    if (scalar @ARGV == 0) {
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2);
    }

    if (scalar @ARGV > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the conjecture arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @ARGV)),
		   -exitval => 2);
    }

    if (! ensure_tptp4x_available ()) {
	print {*STDERR} message (error_message ('Cannot run tptp4X'));
	exit 1;
    }

    my $theory_path = $ARGV[0];

    ensure_sensible_tptp_theory ($theory_path);

    return 1;

}

sub cmd_conjecture {

    conjecture_ensure_sensible_arguments ();

    my $theory_path = $ARGV[0];

    my $theory = Theory->new (path => $theory_path);

    if ($theory->has_conjecture_formula ()) {
	my $conjecture = $theory->get_conjecture ();
	if ($conjecture_opt_name_only) {
	    my $conjecture_name = $conjecture->get_name ();
	    print $conjecture_name;
	} else {
	    my $conjecture_formula = $conjecture->get_formula ();
	    print $conjecture_formula;
	}

	print "\N{LF}";
    }

    return 1;

}

######################################################################
## Model
######################################################################

my $model_opt_with_conjecture_as = undef;
my $model_opt_show_model = 0;
my $model_opt_timeout = 30; # seconds

sub model_ensure_sensible_arguments {

    GetOptions (
	'with-conjecture-as=s' => \$model_opt_with_conjecture_as,
	'show-model' => \$model_opt_show_model,
	'timeout=i' => \$model_opt_timeout,
    )
	or pod2usage (2);

    if (scalar @ARGV == 0) {
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2);
    }

    if (scalar @ARGV > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the model arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @ARGV)),
		   -exitval => 2);
    }

    if ($model_opt_timeout < 0) {
	pod2usage (-msg => error_message ('Invalid value ', $model_opt_timeout, ' for the timeout option.'),
		   -exitval => 2);
    }

    if (defined $model_opt_with_conjecture_as) {
	if ($model_opt_with_conjecture_as ne 'true'
		&& $model_opt_with_conjecture_as ne 'false') {
	    pod2usage (-msg => error_message ('The only permitted values of the --with-conjecture-as option are \'true\' and \'false\'.'),
		       exitval => 2);
	}
    }

    my $theory_path = $ARGV[0];

    ensure_sensible_tptp_theory ($theory_path);

    return 1;

}

sub cmd_model {

    model_ensure_sensible_arguments ();

    my $theory_path = $ARGV[0];

    my $theory = Theory->new (path => $theory_path);

    # Transform the theory
    if (defined $model_opt_with_conjecture_as) {
	if ($model_opt_with_conjecture_as eq 'true') {
	    $theory = $theory->promote_conjecture_to_true_axiom ();
	} else {
	    $theory = $theory->promote_conjecture_to_false_axiom ();
	}
    } else {
	$theory = $theory->strip_conjecture ();
    }

    my $tptp_result = eval { TPTP::find_model ($theory,
					       { 'timeout' => $model_opt_timeout }) };
    my $tptp_result_message = $@;

    if (! defined $tptp_result) {
	print {*STDERR} message (error_message ('Something went wrong searching for a model of ', $theory_path, '.'));
	if (defined $tptp_result_message && $tptp_result_message ne $EMPTY_STRING) {
	    	print {*STDERR} message_with_extra_linefeed ('The error was:');
		print {*STDERR} message ($tptp_result_message);
	} else {
	    print {*STDERR} message ('No further information is available.');
	}

	exit 1;
    }

    if ($tptp_result->timed_out ()) {
	print colored ('Timeout', $BAD_COLOR), "\N{LF}";
	exit 1;
    }

    my $exit_code = $tptp_result->get_exit_code ();

    if (! defined $exit_code || $exit_code != 0) {
	print colored ('Error', $BAD_COLOR);
	exit 1;
    }

    my $model = eval { $tptp_result->output_as_model () };
    my $model_message = $@;

    if (! defined $model) {
	my $tptp_output = $tptp_result->get_output ();
	print {*STDERR} message (error_message ('Something went wrong interpreting the output as a model', $theory_path, ': ', $model_message));
	if (defined $tptp_output && $tptp_output ne $EMPTY_STRING) {
	    	print {*STDERR} message_with_extra_linefeed ('The output we tried to interpret was:');
		print {*STDERR} message ($tptp_output);
	} else {
	    print {*STDERR} message ('Strangely, there was no output at all.  (So how could we possibly interpret it as a model?)');
	}

	exit 1;
    }

    my $szs_status = eval { $tptp_result->get_szs_status () };

    if (! defined $szs_status) {
	my $tptp_output = $tptp_result->get_output ();
	print {*STDERR} message (error_message ('Something went wrong extracting the SZS status for the output of a model-finding task for', $theory_path, '.'));
	if (defined $tptp_output && $tptp_output ne $EMPTY_STRING) {
	    	print {*STDERR} message_with_extra_linefeed ('The output we tried to interpret was:');
		print {*STDERR} message ($tptp_output);
	} else {
	    print {*STDERR} message ('Strangely, there was no output at all.  (So how could we possibly extract the SZS status?)');
	}

	exit 1;
    }

    print colored ($szs_status, $GOOD_COLOR), "\N{LF}"; # ... "It's all good!"

    if ($model_opt_show_model) {
	my $model_description = $model->describe ();
	print $model_description;
    }

    return 1;

}

######################################################################
## Reprove semantically
######################################################################

my $reprove_semantically_opt_timeout = 5;

sub reprove_semantically_ensure_sensible_arguments {

    GetOptions (
	'timeout=i' => \$reprove_semantically_opt_timeout,
    )
	or pod2usage (2);

    if (scalar @ARGV == 0) {
	pod2usage (-msg => error_message ('Please supply a TPTP theory file.'),
		   -exitval => 2);
    }

    if (scalar @ARGV > 1) {
	pod2usage (-msg => error_message ('Unable to make sense of the reprove arguments', "\N{LF}", "\N{LF}", $TWO_SPACES, join ($SPACE, @ARGV)),
		   -exitval => 2);
    }

    if ($reprove_semantically_opt_timeout < 0) {
	pod2usage (-msg => error_message ('Invalid value ', $reprove_semantically_opt_timeout, ' for the timeout option.'),
		   -exitval => 2);
    }

    my $theory_path = $ARGV[0];

    if (! ensure_tptp4x_available ()) {
	print {*STDERR} message (error_message ('Cannot run tptp4X'));
	exit 1;
    }

    ensure_sensible_tptp_theory ($theory_path);

    return 1;

}

sub cmd_reprove_semantically {

    reprove_semantically_ensure_sensible_arguments ();

    my $theory_path = $ARGV[0];

    my $theory = Theory->new (path => $theory_path);

    my $proof_result = eval { TPTP::prove ($theory) };

    if (! defined $proof_result) {
	print {*STDERR} message (warning_message ('We could not verify that the conjecture of ', $theory_path, ' really is a consequence of its axioms.'));
	print {*STDERR} message ('The results that follow may be meaningless.');
    }

    my $proof_szs_status
	= $proof_result->has_szs_status ? $proof_result->get_szs_status : 'Unknown';

    my @axioms = $theory->get_axioms ();

    $theory = $theory->promote_conjecture_to_false_axiom ();

    my %needed = ();

    print 'PREMISES (', colored ('needed', $USED_PREMISE_COLOR), ' / ', colored ('not needed', $UNUSED_PREMISE_COLOR), ' / ', colored ('unknown', $UNKNOWN_COLOR), ')', "\N{LF}";

    foreach my $axiom (@axioms) {
	my $axiom_name = $axiom->get_name ();
	my $trimmed_theory = $theory->remove_formula ($axiom);
	my $tptp_result = eval
	    { TPTP::find_model ($trimmed_theory,
				{ 'timeout' => $reprove_semantically_opt_timeout }) };
	my $tptp_find_model_message = $@;

	my $szs_status
	    = (defined $tptp_result && $tptp_result->has_szs_status ()) ? $tptp_result->get_szs_status () : 'Unknown';

	# DEBUG
	# warn 'SZS status for ', $axiom_name, ': ', $szs_status;

	if (defined $tptp_result) {
	    if ($tptp_result->timed_out ()) {
		print colored ($axiom_name, $UNKNOWN_COLOR);
	    } elsif ($szs_status eq 'Satisfiable') {
		print colored ($axiom_name, $USED_PREMISE_COLOR);
	    } elsif ($szs_status eq 'Unsatisfiable') {
		print colored ($axiom_name, $UNUSED_PREMISE_COLOR);
	    } else {
		print colored ($axiom_name, $UNKNOWN_COLOR);
	    }
	} else {
	    print {*STDERR} message (warning_message ('Something went wrong when testing whether ', $axiom_name, ' can be removed:', "\N{LF}", $tptp_find_model_message));
	}

	print "\N{LF}";
    }

}


__END__
