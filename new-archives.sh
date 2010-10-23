#!/bin/sh

last_change () {
    cd $f;
    x=`git log -1 --format="%ci" | cut -f1 -d' '`
    n=`basename $f`
    cd ..
    echo "$n-$x"
}


for f in ~/lisp/monkeylib/*; do
    last_change
done