package TPTP;

use strict;
use warnings;

require 5.10.0; # for the 'say' feature
use feature 'say';

use base qw(Exporter);
use IPC::Cmd qw(can_run);
use IPC::Run qw(run start timer harness timeout);
use Carp qw(croak carp cluck);
use Readonly;
use Pod::Find qw(pod_where);
use Pod::Usage;
use charnames qw(:full);
use English qw(-no_match_vars);
use Data::Dumper;
use Term::ANSIColor;
use List::MoreUtils qw(any first_value);

our @EXPORT_OK = qw(ensure_tptp4x_available
		    ensure_valid_tptp_file
		    prove_if_possible
		    ensure_sensible_tptp_theory
		    ensure_getsymbols_available
		    known_prover
		    supported_provers
		    incompatible_szs_statuses
		    tptp4X_output);

use Result;
use Utils qw(ensure_readable_file
	     message
	     message_with_extra_linefeed
	     error_message);

Readonly my $TPTP4X => 'tptp4X';
Readonly my $EMPTY_STRING => q{};
Readonly my $DEFAULT_PROVER_TIMEOUT => 30;
Readonly my $TWO_SPACES => q{  };
Readonly my $SPACE => q{ };
Readonly my $FULL_STOP => q{.};
Readonly my $COLON => q{:};
Readonly my $USED_PREMISE_COLOR => 'blue';
Readonly my $UNUSED_PREMISE_COLOR => 'bright_black';
Readonly my $GOOD_COLOR => 'green';
Readonly my $BAD_COLOR => 'red';
Readonly my $GETSYMBOLS => 'GetSymbols';
Readonly my @PROVERS => ('eprover',
			 'vampire',
			 'paradox',
			 'prover9',
			 'mace4',
                         'satallax',);

sub ensure_tptp4x_available {
    return can_run ($TPTP4X);
}

sub ensure_getsymbols_available {
    return can_run ($GETSYMBOLS);
}

sub ensure_valid_tptp_file {
    my $path = shift;

    my @tptp4x_call = ($TPTP4X, '-N', '-c', '-x', $path);
    my $tptp4x_out = $EMPTY_STRING;
    my $tptp4x_err = $EMPTY_STRING;
    my $tptp4x_harness = harness (\@tptp4x_call,
				  '>', \$tptp4x_out,
				  '2>', \$tptp4x_err);

    $tptp4x_harness->start ();
    $tptp4x_harness->finish ();

    my $tptp4x_exit_code = $tptp4x_harness->result (0);

    return ($tptp4x_exit_code == 0 ? 1 : 0);
}

sub tptp4X_output {
    my $path = shift;

    my @tptp4x_call = ($TPTP4X, '-N', '-c', '-x', $path);
    my $tptp4x_out = $EMPTY_STRING;
    my $tptp4x_err = $EMPTY_STRING;
    my $tptp4x_harness = harness (\@tptp4x_call,
				  '>', \$tptp4x_out,
				  '2>', \$tptp4x_err);

    $tptp4x_harness->start ();
    $tptp4x_harness->finish ();

    return $tptp4x_out;
}

sub ensure_sensible_prover_parameters {
    my $parameters_ref = shift;
    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    if (defined $parameters{'timeout'}) {
	my $timeout = $parameters{'timeout'};
	if (defined $timeout) {
	    if ($timeout <= 0) {
		croak 'Error: invalid value ', $timeout, ' for the prover timeout.';
	    } else {
		return 1;
	    }
	} else {
	    croak 'Error: undefined value for timeout.';
	}
    }

    return 1;

}

sub prove {
    my $theory = shift;
    my $prover = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    ensure_sensible_prover_parameters (\%parameters);

    my $timeout = defined $parameters{'timeout'} ? $parameters{'timeout'} : $DEFAULT_PROVER_TIMEOUT;

    my $theory_path = $theory->get_path ();

    if (! can_run ($prover)) {
	croak 'Cannot run', $SPACE, $prover, $FULL_STOP;
    }

    my $output = $EMPTY_STRING;
    my $error = $EMPTY_STRING;
    my $timer = timeout ($timeout);

    my $harness = undef;

    if ($prover eq 'eprover') {
	my @eprover_call = ('eprover',
			    '-l4',
			    '-xAuto',
			    '-tAuto',
			    '-R',
			    '--tptp3-in',
			    "--cpu-limit=${timeout}",
			    '--memory-limit=1024',
			    $theory_path);
	my @epclextract_call
	    = ('epclextract',
	       '--forward-comments');

	$harness = harness (\@eprover_call,
			    '|',  \@epclextract_call,
			    '>',  \$output,
			    '2>', \$error,
			    $timer);

    } elsif ($prover eq 'vampire') {
	my @tptp4X_call = ('tptp4X', '-x', $theory_path);
	my @vampire_call = ('vampire', '-output_axiom_names', 'on', '-mode', 'casc', '-t', $timeout);
	$harness = harness (\@tptp4X_call,
			    '|', \@vampire_call,
			    '>', \$output,
			    '2>', \$error,
			    $timer);
    } elsif ($prover eq 'paradox') {
	my @paradox_call = ('paradox', '--tstp', '--model', $theory_path);
	$harness = harness (\@paradox_call,
			    '>', \$output,
			    '2>', \$error,
			    $timer);
    } elsif ($prover eq 'prover9') {
	my @tptp2X_call = ('tptp2X', '-tstdfof', '-fprover9', '-d-', '-q2', $theory_path);
	my @prover9_call = ('prover9', '-x');
	my @prooftrans_call = ('prooftrans');
	$harness = harness (\@tptp2X_call,
			    '|', \@prover9_call,
			    '|', \@prooftrans_call,
			    '>', \$output,
			    '2>', \$error,
			    $timer);
    } elsif ($prover eq 'mace4') {
	my @tptp2X_call = ('tptp2X', '-tstdfof', '-fprover9', '-d-', '-q2', $theory_path);
	my @mace4_call = ('mace4', '-p', '0', '-P', '1');
	my @interpformat_call = ('interpformat', 'cooked');
	$harness = harness (\@tptp2X_call,
			    '|', \@mace4_call,
			    '|', \@interpformat_call,
			    '>', \$output,
			    '2>', \$error,
			    $timer);
    } else {
	croak 'Unknown prover', $SPACE, $prover, $FULL_STOP;
    }

    eval { $harness->run () };

    until (defined eval { $harness->result () } || $timer->is_expired ()) {
	sleep 1;
    }

    if ($timer->is_expired ()) {
	$harness->kill_kill ();
	return Result->new (timed_out => 1,
			    exit_code => undef,
			    output => $output,
			    error_output => $error,
			    background_theory => $theory,
			    tool => $prover);

    } else {
	my @results = $harness->full_results ();
	my $first_non_zero_exit_code = first_value { $_ != 0 } @results;
	my $exit_code
	    = scalar @results == 0 ? 1 : (defined $first_non_zero_exit_code ?
					      $first_non_zero_exit_code : 0);
	return Result->new (timed_out => 0,
			    exit_code => $exit_code,
			    output => $output,
			    error_output => $error,
			    background_theory => $theory,
			    tool => $prover);
    }

}

sub prove_if_possible {
    my $theory = shift;
    my $prover = shift;
    my $intended_szs_status = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    my $theory_path = $theory->get_path ();
    my $tptp_result = eval { prove ($theory, $prover, $intended_szs_status, \%parameters) };
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
	my $exit_code = $tptp_result->get_exit_code ();
	my $error_output = $tptp_result->get_error_output ();
	print {*STDERR} message (error_message ('The prover terminated, but it did not exit cleanly when working with ', $theory_path, '.'));
	say STDERR 'The exit code was', $SPACE, $exit_code, ', and the error output was:';
	say $error_output;
	exit 1;
    }

    my $szs_status = eval { $tptp_result->get_szs_status () };

    if (! defined $szs_status) {
	my $output = $tptp_result->get_output ();
	print {*STDERR} message (error_message ('We could not find the SZS status of a proof attempt for ', $theory_path, '.'));
	print {*STDERR} message ('The prover output was:', "\N{LF}", "\N{LF}", $output);
	exit 1;
    }

    my $derivation = $tptp_result->output_as_derivation ();

    if (! defined $derivation) {
	print {*STDERR} message (error_message ('We failed to get a derivation.'));
	exit 1;
    }

    return $derivation;

}

sub find_model {

    my $theory = shift;
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    ensure_sensible_prover_parameters (\%parameters);

    my $timeout = defined $parameters{'timeout'} ? $parameters{'timeout'} : $DEFAULT_PROVER_TIMEOUT;

    my $theory_path = $theory->get_path ();

    if (! can_run ('paradox')) {
	croak 'Cannot run paradox.';
    }

    my @paradox_call
	= ('paradox', '--model', '--tstp', $theory_path);

    # warn 'Paradox call: ', Dumper (@paradox_call);

    my $time = timeout ($timeout);

    my $paradox_out = $EMPTY_STRING;
    my $paradox_err = $EMPTY_STRING;
    my $paradox_harness = harness (\@paradox_call,
				   '>', \$paradox_out,
				   '2>', \$paradox_err,
				   $time);

    eval { $paradox_harness->run () };

    if ($time->is_expired ()) {
	$paradox_harness->kill_kill ();
	return Result->new (timed_out => 1,
			    exit_code => undef,
			    output => $paradox_out,
			    error_output => $paradox_err,
			    background_theory => $theory);

    } else {
	my @results = $paradox_harness->full_results ();
	my $exit_code = scalar @results == 0 ? 1 : $results[0];
	return Result->new (timed_out => 0,
			    exit_code => $exit_code,
			    output => $paradox_out,
			    error_output => $paradox_err,
			    background_theory => $theory);
    }

}

sub ensure_sensible_tptp_theory {
    my $theory_path = shift;
    return (ensure_readable_file ($theory_path) && ensure_valid_tptp_file ($theory_path));
}

sub known_prover {
    my $prover = shift;
    return any { $_ eq $prover } @PROVERS;
}

sub supported_provers {
    my @sorted_provers = sort @PROVERS;
    if (wantarray) {
	return @sorted_provers;
    } else {
	return \@sorted_provers;
    }
}

sub incompatible_szs_statuses {
    my $status_1 = shift;
    my $status_2 = shift;

    # we should use the table in Geoff's paper for this

    return 0;

}

__END__

=pod

=head1 NAME

TPTP

=head1 DESCRIPTION

This package contains some utilities for working with TPTP problem
files (theories).

=head1 SEE ALSO

=over 8

=item L<The TPTP and TSTP Quick Guide|http://www.cs.miami.edu/~tptp/TPTP/QuickGuide/>

=back

=cut
