#!/usr/bin/env bash
GRC=./../grace_compiler/gracec.exe

for FILE in /home/c/Documents/Sxolh/Compilers/ntua-compilers/programs/test_suite/tests/scope/*;
do 
    f="$(basename -- $FILE)"
    echo "Running $f"
    if !([[ $f == err* ]]); then
    # Testcase should not produce an error
        if (! $GRC $FILE) &> /dev/null
            then echo "$f failed: Error not expected but happened"
        fi
    elif ($GRC $FILE) &> /dev/null
    # Testcase should produce an error
        then echo "$f failed: Error expected but did not happen"
    fi
done
