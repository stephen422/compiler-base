#!/bin/sh

GREEN="\033[0;32m"
RED="\033[0;31m"
RS="\033[0m"

fail_exit() {
    echo "${RED}FAIL${RS} $1"
    exit 1
}

test() {
    local fail=0
    local ret=0
    local header=$(head -n1 $1)
    echo ${header} | grep -q "fail" && fail=1
    if echo ${header} | grep -q "exit"
    then
	ret=$(echo ${header} | awk '{ print $3 }')
    fi

    rm -f ./out
    build/ruse $1 2>/dev/null
    realret=$?
    if [ "$realret" -eq 0 ]
    then
        ./out
        if [ $? -ne $ret ]
        then
            fail_exit $1
        else
	    echo "${GREEN}PASS${RS} $1"
        fi
    else
        if [ "$fail" -ne 1 ]
        then
            fail_exit $1
        else
	    echo "${GREEN}PASS${RS} $1"
        fi
    fi
}

test test/simple.ruse
test test/return.ruse
test test/if.ruse
# test test/struct2.ruse
# test test/typecheck.ruse
test test/codegen.ruse
test test/struct.ruse
