#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long qw(:config gnu_compat);
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
use TPTP qw(ensure_tptp4x_available ensure_valid_tptp_file);
use Utils qw(ensure_readable_file);

Readonly my $VERSION => qv('1.0');

# Constant strings
Readonly my $EMPTY_STRING => q{};
Readonly my $SPACE => q{ };
Readonly my $TWO_SPACES => q{  };

# Colors
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'bright_black';

Readonly my @COMMANDS => (
    'prove',
    'reprove',
    # 'premises',
    # 'expand',
    # 'init',
    );

Readonly my %COMMAND_ROUTINES =>
    (
	'reprove' => \&cmd_reprove,
	'prove' => \&cmd_prove,
    );
Readonly my %COMMAND_DESCRIPTIONS =>
    (
	'prove'   => 'Try proving a conjecture.',
	'reprove' => 'Repeatedly reprove a conjecture using only actually used premises until no further trimming is possible.',
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

sub error_message {
    my @message_parts = @_;
    my $message = join ($EMPTY_STRING, @message_parts);
    my $message_with_error_padding = colored ('Error', 'red') . ': ' . $message;
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

sub report_initial_and_final_premises {
    my $initial_premises_ref = shift;
    my $final_premises_ref = shift;

    my @initial_premises = @{$initial_premises_ref};
    my @final_premises = @{$initial_premises_ref};

    my @sorted_initial_premises = sort @initial_premises;
    my @sorted_final_premises = sort @final_premises;

    print 'INITIAL PREMISES', "\N{LF}";
    if (scalar @sorted_initial_premises == 0) {
	print '(none)', "\N{LF}";
    } else {
	print join ("\N{LF}", @sorted_initial_premises), "\N{LF}";
    }

    print "\N{LF}", 'FINAL PREMISES', "\N{LF}";
    if (scalar @sorted_final_premises == 0) {
	print '(none)', "\N{LF}";
    } else {
	print join ("\N{LF}", @sorted_final_premises), "\N{LF}";
    }
}

sub report_used_and_unused_premises {
    my $derivation = shift;

    my @unused_premises = $derivation->get_unused_premises ();
    my @used_premises = $derivation->get_used_premises ();

    my @used_premise_names = map { $_->get_name () } @used_premises;
    my @unused_premise_names = map { $_->get_name () } @unused_premises;

    my @sorted_used_premises = sort @used_premise_names;
    my @sorted_unused_premises = sort @unused_premise_names;

    print "\N{LF}", 'USED PREMISES', "\N{LF}";
    if (scalar @sorted_used_premises == 0) {
	print colored ('(none)', $USED_PREMISE_COLOR), "\N{LF}";
    } else {
	my @sorted_used_premises_colored = map { colored ($_, $USED_PREMISE_COLOR) }
	    @sorted_used_premises;
	print join ("\N{LF}", @sorted_used_premises_colored), "\N{LF}";
    }

    print "\N{LF}", 'UNUSED PREMISES', "\N{LF}";
    if (scalar @sorted_unused_premises == 0) {
	print colored ('(none)', $UNUSED_PREMISE_COLOR), "\N{LF}";
    } else {
	my @sorted_unused_premises_colored = map { colored ($_, $UNUSED_PREMISE_COLOR) }
	    @sorted_unused_premises;
	print join ("\N{LF}", @sorted_unused_premises_colored), "\N{LF}";
    }

    return 1;

}

######################################################################
## Prove command
######################################################################

sub prove_ensure_sensible_arguments {

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

    if (! ensure_readable_file ($theory_path)) {
	print {*STDERR} message (error_message ('No such file at ', $theory_path, ' (or the file is unreadable).'));
	exit 1;
    }

    if (! ensure_valid_tptp_file ($theory_path)) {
	print {*STDERR} message (error_message ($theory_path, ' is not a valid TPTP file.'));
	exit 1;
    }

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

    my $derivation = $tptp_result->output_as_derivation ();

    if ($opt_debug) {
	print {*STDERR} 'The derivation was just obtained:', "\N{LF}", Dumper ($derivation);
    }

    print colored ('Success!', 'green'), "\N{LF}";

    report_used_and_unused_premises ($derivation);

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

    if (! ensure_readable_file ($theory_path)) {
	print {*STDERR} message (error_message ('No such file at ', $theory_path, ' (or the file is unreadable).'));
	exit 1;
    }

    if (! ensure_valid_tptp_file ($theory_path)) {
	print {*STDERR} message (error_message ($theory_path, ' is not a valid TPTP file.'));
	exit 1;
    }

    if (! ensure_readable_file ($theory_path)) {
	print {*STDERR} message (error_message ('The supplied file ', $theory_path, ' does not exist or is unreadable.'));
	exit 1;
    }

    return 1;

}

sub prove_if_possible {
    my $theory = shift;

    my $theory_path = $theory->get_path ();
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

    if ($tptp_result->timed_out ()) {
	print {*STDERR} message (error_message ('The prover did not terminate within the time limit.'));
	exit 1;
    }

    if (! $tptp_result->exited_cleanly ()) {
	print {*STDERR} message (error_message ('The prover terminated, but it did not exit cleanly when working with ', $theory_path, '.'));
	exit 1;
    }

    my $derivation = $tptp_result->output_as_derivation ();

    if (! defined $derivation) {
	print {*STDERR} message (error_message ('We failed to get a derivation.'));
	exit 1;
    }

    return $derivation;

}

sub cmd_reprove {

    reprove_ensure_sensible_arguments ();

    my $theory_path = $ARGV[0];

    my $theory = Theory->new (path => $theory_path);

    my $derivation = prove_if_possible ($theory);

    if ($opt_debug) {
	print {*STDERR} 'The derivation was just obtained:', "\N{LF}", Dumper ($derivation);
    }

    my @unused_premises = $derivation->get_unused_premises ();

    if (scalar @unused_premises == 0) {
	print 'No unused premises in the initial proof attempt.', "\N{LF}";
    } else {
	print 'Unused premises:', "\N{LF}";
	while (scalar @unused_premises > 0) {
	    print join ("\N{LF}", @unused_premises), "\N{LF}";
	    $theory = $derivation->theory_from_used_premises ();
	    $derivation = prove_if_possible ($theory);
	    @unused_premises = $derivation->get_unused_premises ();
	}
    }

    return 1;

}
__END__
