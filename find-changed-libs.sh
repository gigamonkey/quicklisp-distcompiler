#!/bin/bash

#
# Find git repos under monkeylib that have been updated since a given
# date or which have uncomitted changes.
#

date=$1

for f in ~/lisp/monkeylib/*; do
    cd $f;
    name=`basename $f`
    x=`git log --log-size --since="$date"`;
    if [ -n "$x" ]; then
        echo "$name: new commits."
    fi;

    x=`git status --porcelain`
    if [ -n "$x" ]; then
        echo "$name: uncommitted changes."
    fi;

    cd ..;
done