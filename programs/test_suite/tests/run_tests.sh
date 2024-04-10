#!/usr/bin/env bash

for FILE in tests/parser/*;
do 
    f="$(basename -- $FILE)"
    echo "Running $f"
    if !([[ $f == err* ]]); then
        # Testcase should not produce an error
        if (! ./gracec $FILE) &> /dev/null
            then echo "$f failed: Error not expected but happened"
        fi
    # Testcase should produce an error
    elif (./gracec $FILE) &> /dev/null
        then echo "$f failed: Error expected but did not happen"
    fi
    # else echo "Success"
done
