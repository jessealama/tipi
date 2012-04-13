#!/usr/bin/env perl

use strict;
use warnings;

require 5.10.0; # for the 'say' feature
use feature 'say';

use Carp qw(carp croak confess);
use Getopt::Long;
use Pod::Usage;
use Readonly;
use charnames qw(:full);
use version;
use Regexp::DefaultFlags;
use List::MoreUtils qw(all);
use Term::ANSIColor;
use IPC::Run qw(run start timer harness timeout);
use List::MoreUtils qw(any);
use Data::Dumper;
use FindBin qw($RealBin);
use lib "$RealBin/../lib";

use Utils qw(error_message
	     ensure_readable_file
	     message_with_extra_linefeed
	     message);
use TPTP qw(ensure_sensible_tptp_theory);

# Commands
use Theory;

Readonly my $VERSION => qv('1.0');

# Constant strings
Readonly my $EMPTY_STRING => q{};
Readonly my $SPACE => q{ };
Readonly my $TWO_SPACES => q{  };

my $opt_man       = 0;
my $opt_help      = 0;
my $opt_verbose   = 0;
my $opt_debug     = 0;
my $opt_version   = 0;
my $opt_timeout   = 30; # seconds
my @opt_provers = ();
my $opt_solution_szs_status = 'Theorem';

sub process_commandline {

    GetOptions(
        'help|?'        => \$opt_help,
        'man'           => \$opt_man,
        'version'       => \$opt_version,
	'debug'         => \$opt_debug,
	'verbose'       => \$opt_verbose,
	'timeout=i'     => \$opt_timeout,
	'with-prover=s' => \@opt_provers,
	'intended-szs-status=s' => \$opt_solution_szs_status,
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

    # debug implies verbose
    if ($opt_debug) {
	$opt_verbose = 1;
    }

    if (scalar @ARGV == 0) {
	pod2usage (-message => 'Please supply a TPTP theory file.',
		   -exitstatus => 2);
    }

    my $theory_path = $ARGV[0];

    if (! ensure_readable_file ($theory_path)) {
	say STDERR error_message ('There is no file at', $SPACE, $theory_path, $SPACE, '(or it is unreadable).');
	exit 1;
    }

    if (! ensure_sensible_tptp_theory ($theory_path)) {
	my $errors = tptp4X_output ($theory_path);
	say {*STDERR} error_message ('The file at ', $theory_path, ' is not a valid TPTP file.');
	say {*STDERR} 'Here is what tptp4X output when evaluating the file:';
	say {*STDERR} $errors;
	exit 1;
    }

    return 1;

}

sub is_running {
    my $harness = shift;
    return (! defined (eval { $harness->results () }));
}

process_commandline ();

my $theory_path = $ARGV[0];
my $theory = Theory->new (path => $theory_path);

my %harnesses = ();

my %parameters = ( 'timeout' => $opt_timeout );

foreach my $prover (@opt_provers) {

    my $coderef = sub
	{ if ($theory->solvable_with ($prover,
				      $opt_solution_szs_status,
				      \%parameters)) {
	    exit 0;
	} else {
	    exit 1;
	}
    };
    my $harness = harness ($coderef);
    $harnesses{$prover} = $harness;
}

my $timer = timer ($opt_timeout);

# Launch all the provers
my @harnesses = values %harnesses;
foreach my $harness (@harnesses) {
    $harness->start ();
}

$timer->start ();

# Wait for a prover to terminate until the clock runs out
until ((! $timer->check ()) || (any { ! $_->pumpable () } @harnesses)) {

    # Pump
    foreach my $harness (@harnesses) {
	if ($harness->pumpable ()) {
	    $harness->pump_nb ();
	}
    }

    sleep 1;

}

# Finish the first nonpumpable harness.  Kill all the others
foreach my $harness (@harnesses) {
    if ($harness->pumpable ()) {
	$harness->kill_kill ();
    } else {
 	$harness->finish ();
    }
 }

# carp 'Harnesses:', $LF, Dumper (%harnesses);

foreach my $prover (keys %harnesses) {
    my $h = $harnesses{$prover};
    my @results = $h->full_results ();
    if (scalar @results == 0) {
	# warn 'Zero results (or ', $prover, ' is still running).';
    } elsif (scalar @results == 1) {
	my $result = $results[0];
	if (defined $result) {
	    if ($result == 0) {
		exit 0;
	    }
	}
    } else {
	# warn 'Huh? Multiple results for ', $prover;
    }
}

exit 1;

__END__
