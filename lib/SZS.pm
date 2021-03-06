package SZS;

use strict;
use warnings;
use base 'Exporter';
use Readonly;
use Carp qw(carp croak confess);
use List::MoreUtils qw(any all);
use Data::Dumper;
use charnames qw(:full);

our @EXPORT_OK = qw(szs_camelword_for
		    known_szs_status
		    is_szs_success
		    szs_implies
		    szs_contradicts
		    szs_status
		    successful_statuses
		    unsuccessful_statuses
		    aggregate_statuses);

Readonly my $SPACE => q{ };
Readonly my $FULL_STOP => q{.};
Readonly my $LF => "\N{LF}";

# Don't know at the moment how to handle ASS

# Success ontology
Readonly my $SUC => 'Success';
Readonly my $UNP => 'UnsatisfiabilityPreserving';
Readonly my $SAP => 'SatisfiabilityPreserving';
Readonly my $ESA => 'EquiSatisfiable';
Readonly my $SAT => 'Satisfiable';
Readonly my $FSA => 'FinitelySatisfiable';
Readonly my $THM => 'Theorem';
Readonly my $EQV => 'Equivalent';
Readonly my $TAC => 'TautologousConclusion';
Readonly my $WEC => 'WeakerConclusion';
Readonly my $ETH => 'EquivalentTheorem';
Readonly my $TAU => 'Tautology';
Readonly my $WTC => 'WeakerTautologousConclusion';
Readonly my $WTH => 'WeakerTheorem';
Readonly my $CAX => 'ContradictoryAxioms';
Readonly my $SCA => 'SatisfiableConclusionContradictoryAxioms';
Readonly my $TCA => 'TautologousConclusionContradictoryAxioms';
Readonly my $WCA => 'WeakerConclusionContradictoryAxioms';
Readonly my $CUP => 'CounterUnsatisfiabilityPreserving';
Readonly my $CSP => 'CounterSatisfiabilityPreserving';
Readonly my $ECS => 'EquiCounterSatisfiable';
Readonly my $CSA => 'CounterSatisfiable';
Readonly my $CTH => 'CounterTheorem';
Readonly my $CEQ => 'CounterEquivalent';
Readonly my $UNC => 'UnsatisfiableConclusion';
Readonly my $WCC => 'WeakerCounterConclusion';
Readonly my $ECT => 'EquivalentCounterTheorem';
Readonly my $FUN => 'FinitelyUnsatisfiable';
Readonly my $UNS => 'Unsatisfiable';
Readonly my $WUC => 'WeakerUnsatisfiableConclusion';
Readonly my $WCT => 'WeakerCounterTheorem';
Readonly my $NOC => 'NoConsequence';

# Non-success ontology
Readonly my $NOS => 'NoSuccess';
Readonly my $OPN => 'Open';
Readonly my $UNK => 'Unknown';
Readonly my $STP => 'Stopped';
Readonly my $ERR => 'Error';
Readonly my $OSE => 'OSError';
Readonly my $INE => 'InputError';
Readonly my $SYE => 'SyntaxError';
Readonly my $SEE => 'SemanticError';
Readonly my $TYE => 'TypeError';
Readonly my $FOR => 'Forced';
Readonly my $USR => 'User';
Readonly my $RSO => 'ResourceOut';
Readonly my $TMO => 'Timeout';
Readonly my $MMO => 'MemoryOut';
Readonly my $GUP => 'GaveUp';
Readonly my $INC => 'Incomplete';
Readonly my $IAP => 'Inappropriate';
Readonly my $INP => 'InProgress';
Readonly my $NTT => 'NotTried';
Readonly my $NTY => 'NotTriedYet';

my %camelword_for =
    (
	'SUC' => $SUC,
	'UNP' => $UNP,
	'SAP' => $SAP,
	'ESA' => $ESA,
	'SAT' => $SAT,
	'FSA' => $FSA,
	'THM' => $THM,
	'EQV' => $EQV,
	'TAC' => $TAC,
	'WEC' => $WEC,
	'ETH' => $ETH,
	'TAU' => $TAU,
	'WTC' => $WTC,
	'WTH' => $WTH,
	'CAX' => $CAX,
	'SCA' => $SCA,
	'TCA' => $TCA,
	'WCA' => $WCA,
	'CUP' => $CUP,
	'CSP' => $CSP,
	'ECS' => $ECS,
	'CSA' => $CSA,
	'CTH' => $CTH,
	'CEQ' => $CEQ,
	'UNC' => $UNC,
	'WCC' => $WCC,
	'ECT' => $ECT,
	'FUN' => $FUN,
	'UNS' => $UNS,
	'WUC' => $WUC,
	'WCT' => $WCT,

	'NOS' => $NOS,
	'OPN' => $OPN,
	'UNK' => $UNK,
	'STP' => $STP,
	'ERR' => $ERR,
	'OSE' => $OSE,
	'INE' => $INE,
	'SYE' => $SYE,
	'SEE' => $SEE,
	'TYE' => $TYE,
	'FOR' => $FOR,
	'USR' => $USR,
	'RSO' => $RSO,
	'TMO' => $TMO,
	'MMO' => $MMO,
	'GUP' => $GUP,
	'INC' => $INC,
	'IAP' => $IAP,
	'INP' => $INP,
	'NTT' => $NTT,
	'NTY' => $NTY,
    );

Readonly my %SUCCESS_CODES =>
    (
	'SUC' => 0,
	'UNP' => 0,
	'SAP' => 0,
	'ESA' => 0,
	'SAT' => 0,
	'FSA' => 0,
	'THM' => 0,
	'EQV' => 0,
	'TAC' => 0,
	'WEC' => 0,
	'ETH' => 0,
	'TAU' => 0,
	'WTC' => 0,
	'WTH' => 0,
	'CAX' => 0,
	'SCA' => 0,
	'TCA' => 0,
	'WCA' => 0,
	'CUP' => 0,
	'CSP' => 0,
	'ECS' => 0,
	'CSA' => 0,
	'CTH' => 0,
	'CEQ' => 0,
	'UNC' => 0,
	'WCC' => 0,
	'ECT' => 0,
	'FUN' => 0,
	'UNS' => 0,
	'WUC' => 0,
	'WCT' => 0,
    );

Readonly my @SUCCESS_CODES => keys %SUCCESS_CODES;

Readonly my %NON_SUCCESS_CODES =>
    (
	'NOS' => 0,
	'OPN' => 0,
	'UNK' => 0,
	'STP' => 0,
	'ERR' => 0,
	'OSE' => 0,
	'INE' => 0,
	'SYE' => 0,
	'SEE' => 0,
	'TYE' => 0,
	'FOR' => 0,
	'USR' => 0,
	'RSO' => 0,
	'TMO' => 0,
	'MMO' => 0,
	'GUP' => 0,
	'INC' => 0,
	'IAP' => 0,
	'INP' => 0,
	'NTT' => 0,
	'NTY' => 0,
    );

Readonly my @NON_SUCCESS_CODES => keys %NON_SUCCESS_CODES;

Readonly my @ALL_SZS_CODES => @SUCCESS_CODES .. @NON_SUCCESS_CODES;

Readonly my %isa => (

    # Success ontology
    $UNP => [],
    $SAP => [],
    $ESA => [$UNP, $SAP],
    $SAT => [$UNP, $SAP, $ESA],
    $THM => [$SAP],
    $EQV => [$UNP, $SAP, $ESA, $SAT, $THM],
    $TAC => [$UNP, $SAP, $ESA, $SAT, $THM],
    $WEC => [$UNP, $SAP, $ESA, $SAT, $THM],
    $ETH => [$UNP, $SAP, $ESA, $SAT, $THM, $EQV],
    $TAU => [$UNP, $SAP, $ESA, $SAT, $THM, $EQV, $TAC],
    $WTC => [$UNP, $SAP, $ESA, $SAT, $THM, $TAC, $WEC],
    $WTH => [$UNP, $SAP, $ESA, $SAT, $THM, $WEC],
    $CAX => [$SAP, $THM],
    $SCA => [$SAP, $THM, $CAX],
    $TCA => [$SAP, $THM, $CAX, $SCA],
    $WCA => [$SAP, $THM, $CAX, $SCA],
    $CSA => [$UNP],
    $UNS => [$UNP, $CSA],
    $NOC => [$UNP, $SAP, $ESA, $SAT, $CSA],

    # No-Success ontology
    $NOS => [],
    $OPN => [$NOS],
    $UNK => [$NOS],
    # don't know how to deal with ASS
    $STP => [$UNK, $NOS],
    $INP => [$UNK, $NOS],
    $NTT => [$UNK, $NOS],
    $ERR => [$STP, $UNK, $NOS],
    $FOR => [$STP, $UNK, $NOS],
    $GUP => [$STP, $UNK, $NOS],
    $NTY => [$NTT, $UNK, $NOS],
    $OSE => [$ERR, $STP, $UNK, $NOS],
    $INE => [$ERR, $STP, $UNK, $NOS],
    $USR => [$FOR, $STP, $UNK, $NOS],
    $RSO => [$FOR, $GUP, $STP, $UNK, $NOS],
    $INC => [$GUP, $STP, $UNK, $NOS],

    # Sutcliffe's 2008 paper includes 'ERR' in two distinct positions
    # in the ongology (ee p. 45); skipping the leaf 'ERR', which would
    # be accounted for this way:
    #
    # $ERR => [$GUP, $STP, $UNK, $NOS],

    $IAP => [$GUP, $STP, $NTT, $UNK, $NOS],
    $SYE => [$INE, $ERR, $STP, $UNK, $NOS],
    $SEE => [$INE, $ERR, $STP, $UNK, $NOS],
    $TMO => [$RSO, $FOR, $GUP, $STP, $UNK, $NOS],
    $MMO => [$RSO, $FOR, $GUP, $STP, $UNK, $NOS],
    $TYE => [$SEE, $INE, $ERR, $STP, $UNK, $NOS],

);

Readonly my %nota => (
    $UNP => [$SAP, $ESA, $SAT, $THM, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CAX, $CSA, $UNS, $NOC],
    $SAP => [$UNP, $ESA, $SAT, $THM, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CAX, $SCA, $TCA, $WCA, $CSA, $NOC],
    $ESA => [$SAT, $THM, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CAX, $CSA, $NOC],
    $SAT => [$THM, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CSA, $NOC],
    $THM => [$UNP, $ESA, $SAT, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CAX, $SCA, $TCA, $WCA],
    $EQV => [$TAC, $ETH, $TAU],
    $TAC => [$EQV, $WEC, $TAU, $WTC],
    $WEC => [$TAC, $WTC, $WTH],
    $ETH => [],
    $TAU => [],
    $WTC => [],
    $WTH => [],
    $CAX => [$UNP, $ESA, $SCA, $TCA, $WCA],
    $SCA => [$TCA, $WCA],
    $TCA => [$EQV],
    $WCA => [],
    $CSA => [$SAP, $ESA, $SAT, $UNS, $NOC],
    $UNS => [],
    $NOC => [],
);

Readonly my %nevera => (
    $UNP => [$TCA, $WCA],
    $SAP => [$UNS],
    $ESA => [$SCA, $TCA, $WCA, $UNS],
    $SAT => [$CAX, $SCA, $TCA, $WCA, $UNS],
    $THM => [$UNS, $NOC],
    $EQV => [$WEC, $WTC, $WTH, $CAX, $SCA, $TCA, $WCA, $CSA, $UNS, $NOC],
    $TAC => [$ETH, $WTH, $CAX, $SCA, $TCA, $WCA, $CSA, $UNS, $NOC],
    $WEC => [$EQV, $ETH, $TAU, $CAX, $SCA, $TCA, $WCA, $CSA, $UNS, $NOC],
    $ETH => [$TAC, $WEC, $TAU, $WTC, $WTH, $CAX, $SCA, $TCA, $WCA, $CSA, $UNS, $NOC],
    $TAU => [$WEC, $ETH, $WTC, $WTH, $CAX, $SCA, $TCA, $WCA, $CSA, $UNS, $NOC],
    $WTC => [$EQV, $ETH, $TAU, $WTH, $CAX, $SCA, $TCA, $WCA, $CSA, $UNS, $NOC],
    $WTH => [$EQV, $TAC, $ETH, $TAU, $WTC, $CAX, $SCA, $TCA, $WCA, $CSA, $UNS, $NOC],
    $CAX => [$SAT, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CSA, $UNS, $NOC],
    $SCA => [$ESA, $SAT, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CSA, $UNS, $NOC],
    $TCA => [$UNP, $ESA, $SAT, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $WCA, $CSA, $UNS, $NOC],
    $WCA => [$UNP, $ESA, $SAT, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $TCA, $CSA, $UNS, $NOC],
    $CSA => [$EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CAX, $SCA, $TCA, $WCA],
    $UNS => [$SAP, $ESA, $SAT, $THM, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CAX, $SCA, $TCA, $WCA, $NOC],
    $NOC => [$THM, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CAX, $SCA, $TCA, $WCA, $UNS],
);

Readonly my %xora => (
    $UNP => [$SCA],
    $SAP => [],
    $ESA => [],
    $SAT => [],
    $THM => [$CSA],
    $EQV => [],
    $TAC => [],
    $WEC => [],
    $ETH => [],
    $TAU => [],
    $WTC => [],
    $WTH => [],
    $CAX => [],
    $SCA => [$UNP],
    $TCA => [],
    $WCA => [],
    $CSA => [$THM],
    $UNS => [],
    $NOC => [],
);

# Invert %camelword_for
my %code_for = ();

foreach my $code (keys %camelword_for) {
    my $camelword = $camelword_for{$code};
    $code_for{$camelword} = $code;
}

sub szs_camelword_for {
    my $code = shift;

    if (defined $camelword_for{$code}) {
	return $camelword_for{$code};
    } else {
	croak 'Unknown SZS code', $SPACE, $code, $FULL_STOP;
    }

}

sub szs_code_for {
    my $camelword = shift;
    if (defined $code_for{$camelword}) {
	return $code_for{$camelword};
    } else {
	croak 'Unknown SZS CamelWord', $SPACE, $camelword, $FULL_STOP;
    }
}

sub is_szs_success_code {
    my $code = shift;
    return (defined $SUCCESS_CODES{$code});
}

sub is_szs_success_camelword {
    my $camelword = shift;
    if (defined $code_for{$camelword}) {
	my $code = $code_for{$camelword};
	return is_szs_success_code ($code);
    } else {
	croak 'The SZS camelword', $SPACE, $camelword, $SPACE, 'is unknown.';
    }
}

sub is_szs_success {
    my $code_or_camelword = shift;
    if (defined $SUCCESS_CODES{$code_or_camelword}) {
	return $code_or_camelword;
    } elsif (defined $code_for{$code_or_camelword}) {
	my $code = $code_for{$code_or_camelword};
	return (defined $SUCCESS_CODES{$code} ? $code : 0);
    } else {
	croak 'Could not determine whether \'', $code_or_camelword, '\' is a known SZS code or CamelWord.';
    }
}

sub is_szs_non_success_code {
    my $code = shift;
    return (defined $NON_SUCCESS_CODES{$code});
}

sub szs_code_implies {
    my $code_1 = shift;
    my $code_2 = shift;
    my $camelword_1 = $camelword_for{$code_1};
    my $camelword_2 = $camelword_for{$code_2};
    if (defined $camelword_1) {
	if (defined $camelword_2) {
	    return szs_camelword_implies ($camelword_1, $camelword_2);
	} else {
	    croak 'Unknown SZS code', $SPACE, $code_2;
	}
    } else {
	croak 'Unknown SZS code', $SPACE, $code_1;
    }
}

sub szs_camelword_implies {
    my $camelword_1 = shift;
    my $camelword_2 = shift;
    if ($camelword_1 eq $camelword_2) {
	return 1;
    } elsif (is_szs_success ($camelword_1) &&
		 is_szs_success ($camelword_2)
		     || (! is_szs_success ($camelword_1) &&
			     ! is_szs_success ($camelword_2))) {
	if (defined $isa{$camelword_1}) {
	    my @isa = @{$isa{$camelword_1}};
	    return (any { $_ eq $camelword_2 } @isa);
	} else {
	    croak 'Unable to determine the SZS ontology isa relation for', $SPACE, $camelword_1, $FULL_STOP;
	}
    } else {
	return 0;
    }
}

sub szs_camelword_contradicts {
    my $camelword_1 = shift;
    my $camelword_2 = shift;
    if (szs_camelword_implies ($camelword_1, $camelword_2)) {
	return 0;
    } elsif (defined $nevera{$camelword_1}) {
	my @nevera = @{$nevera{$camelword_1}};
	if (any { $_ eq $camelword_2 } @nevera) {
	    return 1;
	} elsif (defined $xora{$camelword_1}) {
	    my @xora = @{$xora{$camelword_1}};
	    return (any { $_ eq $camelword_2 } @xora);
	} else {
	    croak 'Unable to look up the SZS CamelWord \'', $camelword_1, '\' in the "xora" table.';
	}
    } else {
	croak 'Unable to look up the SZS CamelWord \'', $camelword_1, '\' in the "nevera" table.';
    }
}

sub szs_contradicts {
    my $szs_code_or_camelword_1 = shift;
    my $szs_code_or_camelword_2 = shift;

    if (defined $camelword_for{$szs_code_or_camelword_1}) {
	my $camelword_1 = $camelword_for{$szs_code_or_camelword_1};
	return szs_contradicts ($camelword_1, $szs_code_or_camelword_2);
    } elsif (defined $camelword_for{$szs_code_or_camelword_2}) {
	my $camelword_2 = $camelword_for{$szs_code_or_camelword_2};
	return szs_contracits ($szs_code_or_camelword_1, $camelword_2);
    } elsif (defined $code_for{$szs_code_or_camelword_1}
		 && defined $code_for{$szs_code_or_camelword_2}) {
	return szs_camelword_contradicts ($szs_code_or_camelword_1,
					  $szs_code_or_camelword_2);
    } elsif (! defined $code_for{$szs_code_or_camelword_1}) {
	confess 'Unable to make sense of the SZS code/camelword \'', $szs_code_or_camelword_1, '\'.';
    } elsif (! defined $code_for{$szs_code_or_camelword_2}) {
	confess 'Unable to make sense of the SZS code/camelword \'', $szs_code_or_camelword_2, '\'.';
    } else {
	confess 'Unable to determine whether SZS code/camelword \'', $szs_code_or_camelword_1, '\' contradicts SZS code/camelword \'', $szs_code_or_camelword_2, '\'.';
    }
}

sub szs_implies {
    my $szs_code_or_camelword_1 = shift;
    my $szs_code_or_camelword_2 = shift;

    if (! defined $szs_code_or_camelword_1) {
	confess 'We seem to be missing a mandatory first argument.';
    }

    if (! defined $szs_code_or_camelword_2) {
	confess 'We seem to be missing a mandatory second argument.';
    }

    if (defined $camelword_for{$szs_code_or_camelword_1}) {
	my $camelword_1 = $camelword_for{$szs_code_or_camelword_1};
	return szs_implies ($camelword_1, $szs_code_or_camelword_2);
    } elsif (defined $camelword_for{$szs_code_or_camelword_2}) {
	my $camelword_2 = $camelword_for{$szs_code_or_camelword_2};
	return szs_implies ($szs_code_or_camelword_1, $camelword_2);
    } elsif (defined $code_for{$szs_code_or_camelword_1}
		 && defined $code_for{$szs_code_or_camelword_2}) {
	return szs_camelword_implies ($szs_code_or_camelword_1,
				      $szs_code_or_camelword_2);
    } elsif (! defined $code_for{$szs_code_or_camelword_1}) {
	confess 'Unable to make sense of the SZS code/camelword \'', $szs_code_or_camelword_1, '\'.';
    } elsif (! defined $code_for{$szs_code_or_camelword_2}) {
	confess 'Unable to make sense of the SZS code/camelword \'', $szs_code_or_camelword_2, '\'.';
    } else {
	confess 'Unable to determine whether SZS code/camelword \'', $szs_code_or_camelword_1, '\' implies SZS code/camelword \'', $szs_code_or_camelword_2, '\'.';
    }

}

sub known_szs_status {
    my $status = shift;
    return (defined $SUCCESS_CODES{$status}
		|| defined $NON_SUCCESS_CODES{$status}
		    || defined $code_for{$status});
}

sub szs_status {
    my $status = shift;
    if (known_szs_status ($status)) {
	return $status;
    } else {
	confess $status, $SPACE, 'is not a known SZS status.';
    }
}

sub successful_statuses {
    my @statuses = @_;
    my @successes = ();
    foreach my $status (@statuses) {
	if (! known_szs_status ($status)) {
	    confess 'Unknown SZS status', $SPACE, $status;
	}
	if (is_szs_success ($status)) {
	    push (@successes, $status);
	}
    }
    if (wantarray) {
	return @successes;
    } else {
	return \@successes;
    }
}

sub unsuccessful_statuses {
    my @statuses = @_;
    my @unsuccesses = ();
    foreach my $status (@statuses) {
	if (! known_szs_status ($status)) {
	    confess 'Unknown SZS status', $SPACE, $status;
	}
	if (! is_szs_success ($status)) {
	    push (@unsuccesses, $status);
	}
    }
    if (wantarray) {
	return @unsuccesses;
    } else {
	return \@unsuccesses;
    }
}

sub compatible_szs_statuses {
    my @statuses = @_;
    my $compatible = 1;
    foreach my $status (@statuses) {
	foreach my $other_status (@statuses) {
	    if (szs_contradicts ($status, $other_status)) {
		$compatible = 0;
		last;
	    }
	}
	if (! $compatible) {
	    last;
	}
    }
    return $compatible;
}

sub maximally_specific_statuses {
    my @statuses = @_;
    my @maximal = ();
    foreach my $status (@statuses) {
	if (all { szs_implies ($status, $_) } @statuses) {
	    push (@maximal, $status);
	}
    }

    # Delete duplicates
    my %maximal = ();
    foreach my $status (@maximal) {
	$maximal{$status} = $status;
    }
    @maximal = keys %maximal;

    if (wantarray) {
	return @maximal;
    } else {
	return \@maximal;
    }
}

sub aggregate_statuses {
    my @statuses = @_;
    my @successes = successful_statuses (@statuses);
    return maximally_specific_statuses (@successes);
}

1;
__END__

=pod

=head1 NAME

SZS

=head1 DESCRIPTION

This package contains utilities for working with SZS statuses.

=head1 SEE ALSO

=over 8

=item L<The SZS Ontology|http://www.cs.miami.edu/~tptp/cgi-bin/SeeTPTP?Category=Documents&File=SZSOntology>

=back

=cut
