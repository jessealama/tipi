#!/bin/bash -

proof=$1
background_theory=$2

if [ -z $proof ]; then
    echo "Usage: `basename $0` EPROVER-PROOF TPTP-BACKGROUND-THEORY";
    exit 1;
fi

if [ -z $background_theory ]; then
    echo "Usage: `basename $0` EPROVER-PROOF TPTP-BACKGROUND-THEORY";
    exit 1;
fi

if [ ! -f $proof ]; then
    echo "Error: the supplied eprover proof '$proof' is not a regular file";
    exit 1;
fi

if [ ! -r $proof ]; then
    echo "Error: the supplied eprover proof '$proof' is not readable";
    exit 1;
fi

if [ ! -f $background_theory ]; then
    echo "Error: the supplied background theory '$background_theory' is not a regular file";
    exit 1;
fi

if [ ! -r $background_theory ]; then
    echo "Error: the supplied background theory '$background_theory' is not readable";
    exit 1;
fi

function used() {
    grep --only-matching ' initial(.*$' $proof \
	| cut -f 2 -d ',' \
	| cut -f 1 -d ')' \
	| sed -e 's/^ //' \
	| sort -u;
}

for formula in `used`; do
    tptp4X -c -x -umachine $background_theory | grep --silent "fof($formula,conjecture," > /dev/null 2>&1;
    if [ $? -ne "0" ]; then echo $formula; fi
done
