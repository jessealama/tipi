#!/bin/bash -

# TPTP theories that have multiple formulas with identical names are
# invalid, even if the contents of the two formulas are identical.
# This script removes such duplicate formulas from an input TPTP
# theory.  The output of this script is not guaranteed to be a TPTP
# theory with non-duplicate names.  One case where this can occur is
# when distinct formulas are assigned the same name, e.g.,
#
# fof(my_formula,axiom,$true).
# fof(my_formula,axiom,$false).
#
# or when a single formula is assigned distinct statuses, e.g.,
#
# fof(my_formula,axiom,$true).
# fof(my_formula,theorem,$false).

theory=$1;

if [ -z $theory ]; then
    echo "Usage: `basename $0` TPTP-THEORY";
    exit 1;
fi

if [ ! -r $theory ]; then
    echo "Error: the supplied TPTP theory file '$theory' is not readable."
    exit 1;
fi

tptp4X -N -c -x -umachine $theory | sort -u | uniq -u
