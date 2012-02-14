#!/bin/bash -

if [ -z $1 ]; then
    echo "Usage: `basename $0` AXIOMS [TIMEOUT] [PROMOTE-CONJECTURES-TO-AXIOMS]";
    exit 1;
fi

axiom_file=$1;

if [ ! -e $axiom_file ]; then
    echo "Error: the specified axiom file '$axiom_file' does not exist.";
    exit 1;
fi

if [ ! -f $axiom_file ]; then
    echo "Error: the specified axiom file '$axiom_file' is not a regular file.";
    exit 1;
fi

if [ ! -r $axiom_file ]; then
    echo "Error: the specified axiom file '$axiom_file' is unreadable.";
    exit 1;
fi

timeout=${2-"30"};

ulimit -t $timeout \
    || (echo "Error: '$timeout' is not an acceptable argument to ulimit -t." && exit 1);

promote_conjectures=${3-"no"};

if [ -z "$3" ]; then
    cat $axiom_file | tptp_to_ladr | mace4 -p 1 -S 1 -m 1 -s "$timeout";
    exit $?;
else
    cat $axiom_file | sed -e 's/,conjecture,/,axiom,/g' | tptp_to_ladr | mace4 -p 1 -S 1 -m 1 -s "$timeout";
    exit $?;
fi
