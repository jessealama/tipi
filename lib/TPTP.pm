package TPTP;

use strict;
use warnings;
use base qw(Exporter);
use IPC::Cmd qw(can_run);
use IPC::Run qw(run start timer harness);
use Carp qw(croak);
use Readonly;

our @EXPORT_OK = qw(ensure_tptp4x_available ensure_valid_tptp_file);

use Result;
use Utils qw(ensure_readable_file);

Readonly my $TPTP4X => 'tptp4X';
Readonly my $EMPTY_STRING => q{};
Readonly my $DEFAULT_PROVER_TIMEOUT => 30;

sub ensure_tptp4x_available {
    return can_run ($TPTP4X);
}

sub ensure_valid_tptp_file {
    my $path = shift;

    my @tptp4x_call = ($TPTP4X, '-N', '-V', '-c', '-x', $path);
    my $tptp4x_out = $EMPTY_STRING;
    my $tptp4x_err = $EMPTY_STRING;
    my $tptp4x_harness = harness (\@tptp4x_call,
				  '>', \$tptp4x_out,
				  '2>', \$tptp4x_err);

    $tptp4x_harness->start ();
    $tptp4x_harness->finish ();

    my $tptp4x_exit_code = $tptp4x_harness->result (0);
    return ($tptp4x_exit_code == 0);
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
    my $parameters_ref = shift;

    my %parameters = defined $parameters_ref ? %{$parameters_ref} : ();

    ensure_sensible_prover_parameters (\%parameters);

    my $timeout = defined $parameters{'timeout'} ? $parameters{'timeout'} : $DEFAULT_PROVER_TIMEOUT;

    my $theory_path = $theory->get_path ();

    if (! can_run ('eprove')) {
	croak 'Cannot run eprove.';
    }

    my @eprover_call = ('eprove');
    my $eprover_out = $EMPTY_STRING;
    my $eprover_err = $EMPTY_STRING;
    my $eprover_harness = harness (\@eprover_call,
				   '<', $theory_path,
				   '>', \$eprover_out,
				   '2>', \$eprover_err,
			           );
    my $timer = timer ($timeout);
    $eprover_harness->run ();
    $timer->start ();

    until (defined eval { $eprover_harness->result () } || $timer->is_expired ()) {
	sleep 1;
    }

    my $timed_out = $timer->is_expired ();
    my $exit_code = defined eval { $eprover_harness->result (0) } ? $eprover_harness->result (0) : undef;

    return Result->new (timed_out => $timed_out,
			exit_code => $exit_code,
			output => $eprover_out,
			error_output => $eprover_err,
		        background_theory => $theory);

}

__END__
