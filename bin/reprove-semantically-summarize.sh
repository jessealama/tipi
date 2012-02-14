#!/bin/bash -

if [ -z "$1" ]; then
    echo "USage: `basename $0` REPROVE-SEMANTICALLY-DIRECTORY";
    exit 1;
fi

reprove_semantically_dir=$1;

if [ ! -d "$reprove_semantically_dir" ]; then
    echo "Error: the supplied reprove-semantically directory, '$reprove_semantically_dir', is not actually a directory.";
    exit 1;
fi

minimal_theory=`find "$reprove_semantically_dir" -maxdepth 1 -mindepth 1 -type f -name "*.maybe-semantically-minimal"`;

if [ -z "$minimal_theory" ]; then
    echo "Error: we were not able to find the generated semantically minimal theory under '$reprove_semantically_dir'.";
    exit 1;
fi

if [ ! -e "$minimal_theory" ]; then
    echo "Error: somehow the generated semantically minimal theory '$minimal_theory' under '$reprove_semantically_dir' doesn't exist.";
    exit 1;
fi

if [ ! -r "$minimal_theory" ]; then
    echo "Error: the semantically minimal theory at '$minimal_theory' is unreadable.";
    exit 1;
fi

minimal_theory_basename=`basename "$minimal_theory"`;
minimal_theory_basename_shorter=`basename "$minimal_theory" .maybe-semantically-minimal`;

paradox_countersatisfiable=0;
mace4_countersatisfiable=0;

# Heuristic for whether a countermodel was found: the paradox model file is nonempty

paradox_countermodel="$reprove_semantically_dir/$minimal_theory_basename.paradox.countermodel";

if [ -e "$paradox_countermodel" ]; then
    if [ ! -r "$paradox_countermodel" ]; then
        echo "Error: the paradox countermodel file at '$paradox_countermodel' is unreadable.";
        exit 1;
    fi
    if [ -s "$paradox_countermodel" ]; then
        paradox_countersatisfiable=1;
    fi
fi

mace4_countermodel_errors="$reprove_semantically_dir/$minimal_theory_basename.mace4.countermodel.errors";

if [ -e "$mace4_countermodel_errors" ]; then
    if [ ! -r "$mace4_countermodel_errors" ]; then
        echo "Error: the mace4 countermodel error output file at '$mace4_countermodel_errors' is unreadable.";
        exit 1;
    fi
    grep --silent 'exit (max_models)' "$mace4_countermodel_errors" > /dev/null 2>&1;
    if [ $? -eq "0" ]; then
        mace4_countersatisfiable=1;
    fi
fi

if [ $mace4_countersatisfiable -eq "1" ]; then
    if [ $paradox_countersatisfiable -eq "1" ]; then
        echo "$minimal_theory_basename_shorter: countersatisfiable (mace4 paradox)";
        exit 0;
    else
        echo "$minimal_theory_basename_shorter: countersatisfiable (mace4)";
        exit 0;
    fi
else
    if [ $paradox_countersatisfiable -eq "1" ]; then
        echo "$minimal_theory_basename_shorter: countersatisfiable (paradox)";
        exit 0;
    fi
fi

# The minimal theory seems to be not countersatisfiable.  Let's find
# the proofs.

vampire_proof="$reprove_semantically_dir/$minimal_theory_basename.vampire.proof";
prover9_proof="$reprove_semantically_dir/$minimal_theory_basename.prover9.proof";
eprover_proof="$reprove_semantically_dir/$minimal_theory_basename.eprover.proof";

if [ ! -e "$vampire_proof" ]; then
    echo "Error: the vampire proof for $minimal_theory_basename doesn't exist.";
    exit 1;
fi

if [ ! -e "$prover9_proof" ]; then
    echo "Error: the prover9 proof for $minimal_theory_basename doesn't exist.";
    exit 1;
fi

if [ ! -e "$eprover_proof" ]; then
    echo "Error: the eprover proof for $minimal_theory_basename doesn't exist.";
    exit 1;
fi

vampire_provable=0;
prover9_provable=0;
eprover_provable=0;

grep --silent 'Refutation found' "$vampire_proof" > /dev/null 2>&1;

if [ $? -eq "0" ]; then
    vampire_provable=1;
fi

grep --silent 'end of proof' "$prover9_proof" > /dev/null 2>&1;

if [ $? -eq "0" ]; then
    prover9_provable=1;
fi

grep --silent "'proof'" "$eprover_proof" > /dev/null 2>&1;

if [ $? -eq "0" ]; then
    eprover_provable=1;
fi

if [ $vampire_provable -eq "1" -a $prover9_provable -eq "1" -a $eprover_provable -eq "1" ]; then
    echo "$minimal_theory_basename_shorter: provable (vampire prover9 eprover)";
elif [ $vampire_provable -eq "1" -a $prover9_provable -eq "1" ]; then
    echo "$minimal_theory_basename_shorter: provable (vampire prover9)";
elif [ $vampire_provable -eq "1" -a $eprover_provable -eq "1" ]; then
    echo "$minimal_theory_basename_shorter: provable (vampire eprover)";
elif [ $vampire_provable -eq "1" ]; then
    echo "$minimal_theory_basename_shorter: provable (vampire)";
elif [ $prover9_provable -eq "1" -a $eprover_provable -eq "1" ]; then
    echo "$minimal_theory_basename_shorter: provable (prover9 eprover)";
elif [ $prover9_provable -eq "1" ]; then
    echo "$minimal_theory_basename_shorter: provable (prover9)";
elif [ $eprover_provable -eq "1" ]; then
    echo "$minimal_theory_basename_shorter: provable (eprover)";
else
    echo "$minimal_theory_basename_shorter: unprovable";
fi

exit 0;
