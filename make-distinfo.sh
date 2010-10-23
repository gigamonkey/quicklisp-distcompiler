#!/bin/bash

date=`date +'%Y-%m-%d'`

cat <<EOF
name: monkeylib
version: $date
system-index-url: http://www.gigamonkeys.com/quicklisp/monkeylib/$date/systems.txt
release-index-url: http://www.gigamonkeys.com/quicklisp/monkeylib/$date/releases.txt
archive-base-url: http://www.gigamonkeys.com/quicklisp/monkeylib/archives/
canonical-distinfo-url: http://www.gigamonkeys.com/quicklisp/monkeylib/$date/distinfo.txt
distinfo-subscription-url: http://www.gigamonkeys.com/quicklisp/monkeylib.txt
EOF
