#!/usr/bin/perl

require v5.10;

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

use FindBin qw($RealBin);
use lib "$RealBin/../lib";

use TPTP;

Readonly my $VERSION => qv('1.0');

# Constant strings
Readonly my $EMPTY_STRING => q{};
Readonly my $SPACE => q{ };
Readonly my $TWO_SPACES => q{  };

Readonly my @COMMANDS => (
    'prove',
    'reprove',
    'premises',
    'expand',
    'init',
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

    my $all_descriptions_known = 1;
    my $all_subroutines_defined = 1;

  DESCRIPTIONS:
    foreach my $command (@COMMANDS) {
	if (! defined $COMMAND_DESCRIPTIONS{$command}) {
	    $all_descriptions_known = 0;
	    last DESCRIPTIONS;
	}
    }

  SUBROUTINES:
    foreach my $command (@COMMANDS) {
	my $command_subroutine = $COMMAND_ROUTINES{$command};
	if (! defined &$command_subroutine) {
	    $all_subroutines_defined = 0;
	    last SUBROUTINES;
	}
    }

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
	my $message = message_with_extra_padding ('Unknown command');
	$message .= message_with_extra_padding ($TWO_SPACES, $command);
	$message .= message_with_extra_padding ('Supported commands:');
	$message .= message (summarize_commands ());
	$message .= message ('See the man page for more details.  (Invoke this program with the option \'--man\'.)');
	pod2usage (-message => error_message ($message),
		   -exitstatus => 2);

    }

    return 1;

}

if (! ensure_sensible_commands ()) {
    print {*STDERR} error_message ('Something is badly wrong with the list of commands; please inform the maintainer.'), "\N{LF}";
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

    return 1;

}

sub cmd_prove {

    reprove_ensure_sensible_arguments ();

    my $theory_path = $ARGV[0];

    my $theory = TPTP->new (path => $theory_path);

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

    return 1;

}

sub cmd_reprove {

    reprove_ensure_sensible_arguments ();

    my $theory_path = $ARGV[0];

    my $theory = TPTP->new ($theory_path);

}
__END__
