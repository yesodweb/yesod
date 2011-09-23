#!/bin/bash

cat << EOF

You're using the deprecated ./test/run.sh. This file will be removed
soon in favor of ../scripts/runtests.

Running ../scripts/runtests...

EOF

../scripts/runtests "$@"
