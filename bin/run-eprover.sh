#!/bin/bash -

theory=$1;
timeout=${2-"30"};

if [ -z $theory ]; then
    echo "Usage: `basename $0` THEORY [TIMEOUT]";
    exit 1;
fi

if [ ! -r $theory ]; then
    echo "Error: the supplied theory '$theory' is not readable.";
    exit 1;
fi

ulimit -t $timeout \
    || (echo "Error: '$timeout' is not an acceptable argument to ulimit -t." && exit 1);

eprover -l4 -xAuto -tAuto -R --tptp3-in $theory \
    | eprover-sentry.pl
