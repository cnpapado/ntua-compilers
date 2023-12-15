#!/usr/bin/env bash

for FILE in /home/apostolis/Shmmy/compilers/grace/yet-another-grace-compiler/tests/runtime/*;
# for FILE in /home/gkosm/Documents/ECE/semester_10/compiler_project/yet-another-grace-compiler/tests/type_checking/*;
do 
    f="$(basename -- $FILE)"
    echo "Running $f"
    if !([[ $f == err* ]]); then
        # Testcase should not produce an error
        if (! ./gracec < $FILE) &> /dev/null
            then echo "$f failed: Error not expected but happened"
        fi
    # Testcase should produce an error
    elif (./gracec < $FILE) &> /dev/null
        then echo "$f failed: Error expected but did not happen"
    fi
    # else echo "Success"
done
