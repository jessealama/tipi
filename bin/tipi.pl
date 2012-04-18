#!/usr/bin/env perl

use strict;
use warnings;

require v5.10.0; # for the 'say' feature
use feature 'say';

use Getopt::Long qw(:config gnu_compat pass_through);
use Pod::Usage;
use Readonly;
use charnames qw(:full);
use version;
use Regexp::DefaultFlags;
use List::MoreUtils qw(all);
use Term::ANSIColor;
use English qw(-no_match_vars);

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
use VegasCommand;
use SelfcheckCommand;
use UsedPremisesCommand;

Readonly my $VERSION => qv('0.9');

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
    'vegas' => VegasCommand->new (),
    'selfcheck' => SelfcheckCommand->new (),
    'used-premises' => UsedPremisesCommand->new (),
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
my $eval_message = $EVAL_ERROR;

if (defined $eval_command) {
    exit 0;
} elsif (defined $eval_message) {
    print {*STDERR} error_message ('Something went wrong executing', $SPACE, $command_name);
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

=pod

=encoding utf8

=head1 NAME

Tipi: TPTP-based theory development environment emphasizing proof analysis

=head1 VERSION

This documentation covers version 0.9 of tipi.

=head1 SYNOPSIS

tipi [--man | --help ]

tipi [command] [options] [file]

=head1 DESCRIPTION

Tipi is a Swiss army knife of theory development/exploration and proof
analysis.

The functionality of tipi is divided into several B<commands>.

There are lots of commands:

=over 8

=item B<prove>

Try proving a conjecture.

=item B<reprove>

Prove a conjecture again, focusing in on needed premises.

=item B<premises>

List the premises of a TPTP theory.

=item B<conjecture>

Print the conjecture(s) of a TPTP problem file.

=item B<model>

Find models of a theory.

=item B<symbols>

Print the symbols occurring in a TPTP theory.

=item B<independence>

Is my set of axioms independent?

=item B<minimize>

Find minimal subtheories of a TPTP theory.

=item B<needed>

Determine whethe a premise is needed.

=item B<countermodel>

Refute a conjecture.

=item B<vegas>

Randomly search for a solution.  Play to Win!

=item B<selfcheck>

Can you even run tipi?

=item B<used-premises>

Extract the used premises from a derivation.

=back

This documentation does not treat the commands in any detail apart
from the short descriptions above.  Each of the commands has its own
arguments and peculiarities.  Nearly all of them operate on a single
TPTP theory/problem file (exceptions: B<selfcheck> and
B<used-premises>).  If you want to learn more about particular
commands, just pass the --help or --man flag to them.  For example, if
you want to know how to call the B<prove> command, try:

=over 4

C<tipi prove --help>

=back

If you want to learn in detail what the B<minimize> command does, try:

=over 4

C<tipi minimize --man>

=back

=head1 DIAGNOSTICS

=head1 CONFIGURATION AND ENVIRONMENT

=head1 DEPENDENCIES

Tipi has, unfortunately, a number of wide-ranging dependencies.

If the dependencies are too heavy for you, please let me know.  If you
cannot run perl 5.10 (or newer), or you cannot install the needed CPAN
modules (not even "locally", as a non-superuser/non-administrator of
your computer), then I may be able to help (see L</"AUTHOR").

Since the depedencies of tipi are somewhat steep, I have half a mind
to prove a web-based front-end to tipi.  Such an interface would cut
down the dependencies dramatically; all that would be needed then is a
web browser.  If you would be interested in having such an interface
to tipi, write to me to discuss (see L</"AUTHOR">).

=head2 Perl dependences

=head2 Dependencies on automated reasoning packages

=head2 Dependencies coming from the TPTP World

=head1 INCOMPATIBILITIES

=head1 HISTORY

Tipi grew out of some theory development work with Ed Zalta and Paul
Oppenheimer of Stanford University.  They were doing some
theory-development work for their computational metaphysics research
(see L<their homepage|http://mally.stanford.edu/cm/>) and wanted some
help.  As I (Jesse Alama) helped them, I saw that some of the
questions and problems that kept coming up could, in part, be
mechanized.  I was initially doing some work in Emacs to facilitate
operating on TPTP files (at one point I was even hacking applescript
en route toward a "TPTP-mode" for the collaborative text editor
L<SubEthaEdit|http://www.codingmonkeys.de/subethaedit/>).  Eventually
it became clear that a lot of work needed to be done.  I cooked up
some bash shell scripts for extracting from Vampire, Eprover, and
Prover9 derivations and doing some other initial experiments.  I had
some perl scripts for doing some stuff that seemed to hard to do as a
shell script.  Eventually I got tired of the difficulty of maintaining
a consistent collection of shell scripts and the hybrid shell/perl
approach.  So I decided to start from scratch with a more or less
purely Perl-based implementation.  The result is Tipi.

=head1 BUGS AND LIMITATIONS

=head1 AUTHOR

Jesse Alama L<jesse.alama@gmail.com>.

=head1 SEE ALSO

=over 8

=item L<The TPTP Homepage|http://www.tptp.org>

=item L<The E Theorem Prover|http://www.eprover.org>

=item L<The Vampire Theorem Prover|http://www.vprover.org>

=item L<Prover9, Mace4, LADR, etc.|http://www.cs.unm.edu/~mccune/prover9/>

=back

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2012 Jesse Alama (jesse.alama@gmail.com).  All rights
reserved.

This module is free software; you can redistribute it and/or modify it
under the same terms as Perl itself. See L<perlartistic>.  This
program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.

=cut
