package SZS;

use base 'Exporter';
use Readonly;
use Carp qw(carp croak);
use List::MoreUtils qw(any);

our @EXPORT_OK = qw(szs_camelword_for
		    is_szs_success_code
		    is_szs_success_camelword);

Readonly my $SPACE => q{ };
Readonly my $FULL_STOP => q{.};

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
Readonly my $SCA => 'SatisfiableConclusionContradictoryAxioms',
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
);

Readonly my %nota => (
    $UNP => [$SAP, $ESA, $SAT, $THM, $EQV, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CAX, $CSA, $UNS, $NOC],
    $SAP => [$UNP, $ESA, $SAT, $THM, $EQC, $TAC, $WEC, $ETH, $TAU, $WTC, $WTH, $CAX, $SCA, $TCA, $WCA, $CSA, $NOC],
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
	    if ($camelword_1 eq $camelword_2) {
		return 1;
	    } else {
		if (defined $isa{$camelword_1}) {
		    my @isa = @{$isa{$camelword_1}};
		    return (any { $_ eq $camelword_2 } @isa);
		} else {
		    croak 'Unable to determine the SZS ontology isa relation for', $SPACE, $camelword_1, $FULL_STOP;
		}
	    }
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
}

1;
__END__
