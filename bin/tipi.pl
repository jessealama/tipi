#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long qw(:config gnu_compat);
use Pod::Usage;
use Readonly;
use charnames qw(:full);
use English qw(-no_match_vars);
use version;
use Carp qw(croak);
use Term::ANSIColor;

Readonly my $VERSION => qv('1.0');

Readonly my %COMMANDS =>
    ('reprove' => \&cmd_reprove);
Readonly my %COMMAND_DESCRIPTIONS =>
    ('reprove' => 'Repeatedly reprove a conjecture using only actually used premises until no further trimming of the premises is possible.');

my $opt_man       = 0;
my $opt_help      = 0;
my $opt_verbose   = 0;
my $opt_debug     = 0;
my $opt_version   = 0;

sub ensure_sensible_commands {

    my $all_descriptions_known = 1;

  COMMANDS:
    foreach my $command (keys %COMMANDS) {
	if (! defined $COMMAND_DESCRIPTIONS{$command}) {
	    $all_descriptions_known = 0;
	    last COMMANDS;
	}
    }

    return $all_descriptions_known;

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
        print $VERSION, "\N{LF}";
        exit 0;
    }

    # debug implies verbose
    if ($opt_debug) {
        $opt_verbose = 1;
    }

    if (scalar @ARGV == 0) {
	pod2usage (2);
    }

    my $command = $ARGV[0];

    if (! defined $COMMANDS{$command}) {
	print {*STDERR} colored ('Error', 'red'), ': unknown command', "\n", "\n", '  ', $command, "\n", "\n";
	print {*STDERR} 'Supported commands:', "\n", "\n";
	foreach my $command (keys %COMMANDS) {
	    my $description = $COMMAND_DESCRIPTIONS{$command};
	    print {*STDERR} '  * ', colored ($command, 'blue'), ' -- ', $description, "\n";
	}
	print {*STDERR} "\n", 'See the man page for more details.  (Invoke this program with the option \'--man\'.)', "\n";
    }

    return 1;

}

my $commands_ok = ensure_sensible_commands ();

if (! $commands_ok) {
    print {*STDERR} 'Error: something is badly wrong with the list of commands; please inform the maintainer.', "\n";
    exit 1;
}

process_commandline ();

__END__
