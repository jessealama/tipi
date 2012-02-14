#!/usr/bin/perl -w

use strict;

use Term::ANSIColor;

use File::Basename qw(basename dirname);

use POSIX qw(floor);

sub copy_string {
  my $string = shift;
  my $num_copies = shift;
  my $final_string = '';
  foreach my $i (1 .. $num_copies) {
    $final_string .= $string;
  }
  return $final_string;
}

sub max {
  my $a = shift;
  my $b = shift;
  $a > $b ? $a : $b;
}

sub extract_formula_from_fof {
  my $fof = shift;
  if (defined $fof) {
    # the next pattern is not robust: it works only for some TPTP fof
    # lines, not all.  It assumes that the fof line has exactly three
    # parts: the formula name, the formula status, and the formula
    # proper.  But TPTP syntax permits multiple fields after the
    # formula proper.  Do we need a TPTP parser?  *sigh*
    $fof =~ m/fof\([^,]+,[^,]+,(.+)\).$/;
    my $formula = $1;
    # sanity check
    if (defined $formula) {
      return $formula;
    } else {
      warn "We were unable to extract the formula from '$fof'";
      return '';
    }
  } else {
    die "We were given an empty argument to extract_formula_from_fof";
  }
}

sub extract_name_from_fof {
  my $fof = shift;
  $fof =~ m/fof\(([^,]+),/;
  my $name = $1;
  # sanity check
  if (defined $name) {
    return $name;
  } else {
    warn "We were unable to extract the formula name from '$fof'";
    return '';
  }
}

if (scalar @ARGV == 0) {
  print 'Usage: reprove-semantically TPTP-THEORY', "\n";
  exit 1;
}

if (scalar @ARGV > 1) {
  print 'Usage: reprove-semantically TPTP-THEORY', "\n";
  exit 1;
}

my $tptp_theory = $ARGV[0];
my $tptp_theory_basename = basename ($tptp_theory);
my $tptp_theory_dirname = dirname ($tptp_theory);

# Strip extension, if present

$tptp_theory_basename =~ m/(.+)\.(.*)$/;

(my $basename_before_final_period, my $extension) = ($1, $2);

if (defined $extension) {
  $tptp_theory_basename = $basename_before_final_period;
}

if (! -e $tptp_theory) {
  print 'Error: the supplied file ', $tptp_theory, ' does not exist.', "\n";
  exit 1;
}

if (-d $tptp_theory) {
  print 'Error: the supplied file ', $tptp_theory, ' is actually a directory.', "\n";
  exit 1;
}

if (! -r $tptp_theory) {
  print 'Error: the TPTP theory at ', $tptp_theory, ' is unreadable.', "\n";
  exit 1;
}

# check for presence of tptp4X

my $which_tptp4X_status = system ("which tptp4X > /dev/null 2>&1");
my $which_tptp4X_exit_code = $which_tptp4X_status >> 8;

if ($which_tptp4X_exit_code != 0) {
  print 'Error: the required program tptp4X could not be found in your path.', "\n";
  exit 1;
}

my @conjecture_fofs
  = `tptp4X -N -V -c -x -umachine $tptp_theory | grep ',conjecture,'`;
chomp @conjecture_fofs;
my @non_conjecture_fofs
  = `tptp4X -N -V -c -x -umachine $tptp_theory | grep --invert-match ',conjecture,'`;
chomp @non_conjecture_fofs;

if (scalar @conjecture_fofs > 1) {
  print 'Error: there is more than one conjecture formula in the given theory.', "\n";
  exit 1;
}

my $conjecture_fof
  = scalar @conjecture_fofs == 0 ? undef : $conjecture_fofs[0];
my $conjecture = undef;
my $negated_conjecture = undef;

# explicitly negate the conjecture, if there is one
if (defined $conjecture_fof) {
  $conjecture = extract_formula_from_fof ($conjecture_fof);
  if ($conjecture eq '') {
    warn "Unable to extract the formula from the conjecture fof!";
  } else {
    $negated_conjecture = '~ ( ' . $conjecture . ')';
  }
}

my @non_conjecture_formulas = map { extract_formula_from_fof($_); } @non_conjecture_fofs;
my @non_conjecture_names = map { extract_name_from_fof($_); } @non_conjecture_fofs;

my %needed_formulas = ();

my $length_of_longest_premise = 0;

foreach my $premise (@non_conjecture_names) {
  my $len = length $premise;
  if ($len > $length_of_longest_premise) {
    $length_of_longest_premise = $len;
  }
}

my $bigger = max (length ('Premise'), $length_of_longest_premise);
my $padding = $bigger == length ('Premise') ? 0 : $length_of_longest_premise - length ('Premise');

print copy_string ('=', length ('Premise') + $padding + length (' | ') + length ('Needed according to mace4') + length (' | ') + length ('Needed according to paraox') + 1), "\n";
print 'Premise', copy_string (' ', $padding), ' | ', 'Needed according to mace4', ' | ', 'Needed according to paradox', "\n";
print copy_string ('=', length ('Premise') + $padding + length (' | ') + length ('Needed according to mace4') + length (' | ') + length ('Needed according to paraox') + 1), "\n";

# Make the directory where we'll save our work

my $candidate_number = 1;
my $work_directory = "$tptp_theory_dirname/$tptp_theory_basename-reprove-semantically-$candidate_number";

while (-d $work_directory) {
  $candidate_number++;
  $work_directory = "$tptp_theory_dirname/$tptp_theory_basename-reprove-semantically-$candidate_number";
}

mkdir $work_directory
  or die "Unable to make the work directory at '$work_directory'";

foreach my $i (1 .. scalar @non_conjecture_formulas) {
  my $non_conjecture_fof = $non_conjecture_fofs[$i-1];
  my $non_conjecture_name = $non_conjecture_names[$i-1];
  my $dir_for_non_conjecture = "$work_directory/$non_conjecture_name";
  mkdir $dir_for_non_conjecture;
  my $ad_hoc_theory_path = "$dir_for_non_conjecture/$tptp_theory_basename-without-$non_conjecture_name.tptp";
  open (AD_HOC_THEORY, '>', $ad_hoc_theory_path) or die "Can't open an output filehandle for '$ad_hoc_theory_path'";
  foreach my $j (1 .. scalar @non_conjecture_formulas) {
    unless ($i == $j) {
      print AD_HOC_THEORY ('fof(' . "ax$j" . ',axiom,' . $non_conjecture_formulas[$j-1] . ').' . "\n");
    }
  }
  if (defined $negated_conjecture) {
    print AD_HOC_THEORY ('fof(our_conjecture,axiom,' . $negated_conjecture . ').' . "\n");
  }
  close AD_HOC_THEORY
    or die "Can't close the output filehandle for splork";
  my $length_of_this_premise = length $non_conjecture_name;
  my $padding = $length_of_longest_premise - $length_of_this_premise;
  print colored ($non_conjecture_name, 'blue'), copy_string (' ', $padding), ' | ';

  my $ad_hoc_mace4_countermodel_path = "$dir_for_non_conjecture/$tptp_theory_basename-without-$non_conjecture_name.mace4-model";
  my $ad_hoc_mace4_countermodel_errors = "$dir_for_non_conjecture/$tptp_theory_basename-without-$non_conjecture_name.mace4-errors";
  my $mace4_status = system ("run-mace4.sh $ad_hoc_theory_path 5 > $ad_hoc_mace4_countermodel_path 2> $ad_hoc_mace4_countermodel_errors");
  my $mace4_exit_code = $mace4_status >> 8;
  if ($mace4_exit_code == 0) {
    print colored ('needed', 'red'), '                   ';
    $needed_formulas{$non_conjecture_fof} = 0;
  } else {
    print colored ('unknown', 'cyan'), '      ';
  }

  print ' | ';

  my $ad_hoc_paradox_countermodel_path = "$dir_for_non_conjecture/$tptp_theory_basename-without-$non_conjecture_name.paradox-model";
  my $ad_hoc_paradox_countermodel_errors = "$dir_for_non_conjecture/$tptp_theory_basename-without-$non_conjecture_name.paradox-errors";

  my $paradox_status = system ("run-paradox.sh $ad_hoc_theory_path 5 > $ad_hoc_paradox_countermodel_path 2> $ad_hoc_paradox_countermodel_errors");
  my $paradox_exit_code = $paradox_status >> 8;
  if ($paradox_exit_code == 4) {
    print colored ('needed', 'red');
    $needed_formulas{$non_conjecture_fof} = 0;
  } else {
    print colored ('unknown', 'cyan');
  }

  if ($i == scalar @non_conjecture_fofs) {
    print "\n";
  } else {
    print "\n";
    print copy_string ('-', $length_of_longest_premise + length (' | ') + length ('Needed according to mace4') + length (' | ') + length ('Needed according to paraox') + 1), "\n";
}


}

print copy_string ('=', length ('Premise') + $padding + length (' | ') + length ('Needed according to mace4') + length (' | ') + length ('Needed according to paraox') + 1), "\n";

# Test whether the formulas that we found were needed suffice for a proof

my $maybe_minimal_theory = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal";

open (MINIMAL_THEORY, '>', $maybe_minimal_theory)
  or die "Can't open '$maybe_minimal_theory'";

foreach my $fof (keys %needed_formulas) {
  print MINIMAL_THEORY ("$fof\n");
}

if (defined $conjecture) {
  print MINIMAL_THEORY ('fof(our_conjecture,conjecture,' . $conjecture . ').' . "\n");
}

close MINIMAL_THEORY
  or die "Can't close '$maybe_minimal_theory'";

# print "Now we will test whether the 'semantically minimal' subtheory of the original theory suffices to prove the conjecture.\n";

# The semantically minimal theory might be countersatisfiable

print '=' x (length ('Premise') + $padding + length (' | ') + length ('Needed according to mace4') + length (' | ') + length ('Needed according to paraox') + 1), "\n";
print " Quick Check: Is the 'semantically minimal' theory countersatisfiable?", "\n";
print '=' x (length ('Premise') + $padding + length (' | ') + length ('Needed according to mace4') + length (' | ') + length ('Needed according to paraox') + 1), "\n";

# print 'We will now use a model finder to check whether the theory we just constructed is countersatisfiable.', "\n";

my $minimal_theory_countersatisfiable = undef;

print '* mace4.....';

my $mace4_countermodel = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal.mace4.countermodel";
my $mace4_countermodel_errors = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal.mace4.countermodel.errors";

my $mace4_status = system ("run-mace4.sh $maybe_minimal_theory 5 > $mace4_countermodel 2> $mace4_countermodel_errors");
my $mace4_exit_code = $mace4_status >> 8;

if ($mace4_exit_code == 0) {
  print colored ('countersatisfiable', 'red'), "!\n";
  $minimal_theory_countersatisfiable = 1;
} else {
  print colored ('unknown', 'cyan'), "\n";
  print '* paradox...';
  my $paradox_countermodel = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal.paradox.countermodel";
  my $paradox_countermodel_errors = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal.paradox.countermodel.errors";
  my $paradox_status = system ("run-paradox.sh $maybe_minimal_theory 5 > $paradox_countermodel 2> $paradox_countermodel_errors");
  my $paradox_exit_code = $paradox_status >> 8;
  if ($paradox_exit_code == 2) {
    print colored ('countersatisfiable', 'red'), "!\n";
    $minimal_theory_countersatisfiable = 1;
  } else {
    print colored ('unknown', 'cyan'), "\n";

    if (defined $minimal_theory_countersatisfiable) {
      print "Our 'semantically minimal' theory is countersatisfiable; it does not suffice to prove the conjecture.\n";
      print "Some further principle is needed.";
      if ($mace4_exit_code == 0 && $paradox_exit_code == 2) {
        print "Consult the countermodels at\n\n  $mace4_countermodel\n\nand\n\n  $paradox_countermodel\n\nto see what is missing.\n";
      } elsif ($mace4_exit_code == 0) {
        print "Consult the countermodel at\n\n  $mace4_countermodel\n\nto see what is missing.\n";
      } elsif ($paradox_exit_code == 2) {
        print "Consult the countermodel at\n\n  $paradox_countermodel\n\nto see what is missing.\n";
      }
    } else {

      print '=' x (length ('Premise') + $padding + length (' | ') + length ('Needed according to mace4') + length (' | ') + length ('Needed according to paraox') + 1), "\n";
      print "   Deriving the conjecture from the 'semantically minimal' premises", "\n";
      print '=' x (length ('Premise') + $padding + length (' | ') + length ('Needed according to mace4') + length (' | ') + length ('Needed according to paraox') + 1), "\n";

      my $eprover_countersatisfiable = undef;
      my $vampire_countersatisfiable = undef;
      my $prover9_countersatisfiable = undef;

      print '* eprover...';
      my $eprover_proof = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal.eprover.proof";
      my $eprover_proof_errors = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal.eprover.proof.errors";
      my $eprover_status = system ("run-eprover.sh $maybe_minimal_theory > $eprover_proof 2> $eprover_proof_errors");
      my $eprover_exit_code = $eprover_status >> 8;
      if ($eprover_exit_code == 0) {
        print colored ('confirmed', 'green'), '!', "\n";
      } elsif ($eprover_exit_code == 2) {
        $eprover_countersatisfiable = 0;
        print colored ('countersatisfiable', 'red'), "\n";
      } else {
        print colored ('unknown', 'cyan'), "\n";
      }

      print '* vampire...';
      my $vampire_proof = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal.vampire.proof";
      my $vampire_proof_errors = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal.vampire.proof.errors";
      my $vampire_status = system ("run-vampire.sh $maybe_minimal_theory > $vampire_proof 2> $vampire_proof_errors");
      my $vampire_exit_code = $vampire_status >> 8;
      if ($vampire_exit_code == 0) {
        print colored ('confirmed', 'green'), '!', "\n";
      } elsif ($vampire_exit_code == 2) {
        $vampire_countersatisfiable = 0;
        print colored ('countersatisfiable', 'red'), "!  (Some further principle is needed.  No countermodel was provided directly; see $vampire_proof)", "\n";
      } else {
        print colored ('unknown', 'cyan'), "\n";
      }

      print '* prover9...';
      my $prover9_proof = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal.prover9.proof";
      my $prover9_proof_errors = "$work_directory/$tptp_theory_basename.maybe-semantically-minimal.prover9.proof.errors";
      my $prover9_status = system ("run-prover9.sh $maybe_minimal_theory > $prover9_proof 2> $prover9_proof_errors");
      my $prover9_exit_code = $prover9_status >> 8;
      if ($prover9_exit_code == 0) {
        print colored ('confirmed', 'green'), '!', "\n";
      } elsif ($prover9_exit_code == 2) {
        $prover9_countersatisfiable = 0;
        print colored ('countersatisfiable', 'red'), "\n";
      } else {
        print colored ('unknown', 'cyan'), "\n";
      }

      if (defined $eprover_countersatisfiable or defined $vampire_countersatisfiable or defined $prover9_countersatisfiable) {
        print "\n";
        print "At least one theorem prover found that that the conjecture is\ncountersatisfiable in the 'semantically minimal' theory.\n";
        print 'Our theorem provers do not directly provide a countermodel, but you may\nconsult the following proofs to see what went wrong:', "\n";
        print "\n";
        if (defined $eprover_countersatisfiable) {
          print "* eprover: ", basename ($eprover_proof), "\n";
        }
        if (defined $vampire_countersatisfiable) {
          print "* vampire: ", basename ($vampire_proof), "\n";
        }
        if (defined $prover9_countersatisfiable) {
          print "* prover9: ", basename ($prover9_proof), "\n";
        }

        print "\n";
      }

      print '=' x (length ('Premise') + $padding + length (' | ') + length ('Needed according to mace4') + length (' | ') + length ('Needed according to paraox') + 1), "\n";
    }
  }
}

print "Done.  Our work has been saved under $work_directory.\n";

exit 0;
