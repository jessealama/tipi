#!/bin/bash -

eprover_proof=$1;
theory=$2;

if [ -z $eprover_proof ]; then
    echo "Usage: `basename $0` EPROVER-PROOF-OBJECT TPTP-BACKGROUND-THEORY";
    exit 1;
fi

if [ -z $theory ]; then
    echo "Usage: `basename $0` EPROVER-PROOF-OBJECT TPTP-BACKGROUND-THEORY";
    exit 1;
fi

if [ ! -r $eprover_proof ]; then
    echo "Error: unable to read the eprover proof '$eprover_proof'";
    exit 1;
fi

if [ ! -r $theory ]; then
    echo "Error: unable to read the TPTP theory '$theory'";
    exit 1;
fi

for formula in `tptp4X -V -N -c -x -umachine $theory | grep --invert-match ',conjecture,' | cut -f 1 -d ',' | sed -e 's/fof(//'`; do
    grep --silent " initial(.*, $formula)" $eprover_proof > /dev/null 2>&1;
    if [ $? -ne "0" ]; then echo $formula; fi
done