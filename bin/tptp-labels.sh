#!/bin/bash -

tptp4X -x -c -umachine $1 \
    | cut -f 1 -d ',' \
    | sed -e 's/fof(//'
