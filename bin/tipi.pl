#!/usr/bin/env perl

use strict;
use warnings;

require 5.10.0; # for the 'say' feature
use feature 'say';

use Getopt::Long qw(:config gnu_compat pass_through);
use Pod::Usage;
use Readonly;
use charnames qw(:full);
use version;
use Regexp::DefaultFlags;
use List::MoreUtils qw(all);
use Term::ANSIColor;

use FindBin qw($RealBin);
use lib "$RealBin/../lib";

use Utils qw(error_message
	     message_with_extra_linefeed
	     message);

# Commands
use ProveCommand;
use ReproveCommand;
use PremisesCommand;
use ConjectureCommand;
use ModelCommand;
use SymbolsCommand;
use IndependenceCommand;
use MinimizeCommand;
use NeededCommand;
use CountermodelCommand;

Readonly my $VERSION => qv('1.0');

# Constant strings
Readonly my $EMPTY_STRING => q{};
Readonly my $SPACE => q{ };
Readonly my $TWO_SPACES => q{  };

Readonly my %COMMANDS => (
    'prove' => ProveCommand->new (),
    'reprove' => ReproveCommand->new (),
    'premises' => PremisesCommand->new (),
    'conjecture' => ConjectureCommand->new (),
    'model' => ModelCommand->new (),
    'symbols' => SymbolsCommand->new (),
    'independence' => IndependenceCommand->new (),
    'minimize' => MinimizeCommand->new (),
    'needed' => NeededCommand->new (),
    'countermodel' => CountermodelCommand->new (),
);

my $opt_man       = 0;
my $opt_help      = 0;
my $opt_verbose   = 0;
my $opt_debug     = 0;
my $opt_version   = 0;

sub summarize_commands {

    my $summary = $EMPTY_STRING;

    foreach my $command_name (sort keys %COMMANDS) {
	my $command = $COMMANDS{$command_name};
	my $description = $command->describe ();
	$summary .= message ('  * ', colored ($command_name, 'blue'), ' -- ', $description);
    }

    return $summary;

}

sub process_commandline {

    GetOptions(
        'help|?'      => \$opt_help,
        'man'         => \$opt_man,
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
        say $VERSION;
        exit 0;
    }

    if (scalar @ARGV == 0) {
	my $message = message_with_extra_linefeed ('Please supply a tipi command.  The available commands are:');
	$message .= message (summarize_commands ());
	$message .= message ('See the man page for more information.  (Invoke this program with the option \'--man\'.)');
	pod2usage (-message => $message,
		   -exitstatus => 2);
    }

    my $command = $ARGV[0];

    if (! defined $COMMANDS{$command}) {
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

process_commandline ();

my $command_name = $ARGV[0];
my $command = $COMMANDS{$command_name};
shift @ARGV;

my $eval_command = eval { $command->execute (@ARGV) };
my $eval_message = $@;

if (defined $eval_command) {
    exit 0;
} elsif (defined $eval_message) {
    print {*STDERR} error_message ('Something went wrong executing the command \'', $command_name, '\'');
    if (scalar @ARGV == 0) {
	say STDERR (' (without any further arguments).');
    } else {
	my $argument_list = join ($SPACE, @ARGV);
	say STDERR (' with the arguments', "\N{LF}");
	say STDERR ($TWO_SPACES, $argument_list, "\N{LF}");
    }
    if ($eval_message eq $EMPTY_STRING) {
	say 'Somehow we have no error output to report.';
    } else {
	say STDERR ('The error was:', "\N{LF}");
	say STDERR $eval_message;
    }
    say STDERR 'Please inform the maintainers.';
    exit 1;
} else {
    say STDERR error_message ('Something went badly wrong trying to execute the command \'', $command_name, '\'.');
    say STDERR 'Please inform the maintainers.';
    exit 1;
}
__END__
