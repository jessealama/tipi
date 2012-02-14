#!/bin/bash -

theory=$1;
timeout=${2-"30"};

ulimit -t $timeout \
    || (echo "Error: '$timeout' is not an acceptable argument to ulimit -t." && exit 1);

# This transformation using tptp_to_ladr is not logically correct.  We
# need to use something like tptp2X here, not tptp_to_ladr.  Until we
# fix this problem, do not trust the output of this script.

tptp2X -tstdfof -fprover9 -d- -q2 $theory \
    | prover9 -x 2>&1 \
    | prover9-sentry.pl;
