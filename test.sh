#!/bin/sh

GREEN="\033[0;32m"
RED="\033[0;31m"
RS="\033[0m"

test() {
    local ret=0
    local header=$(head -n1 $1)
    echo ${header} | grep -q "fail" && ret=1
    if echo ${header} | grep -q "exit"
    then
	ret=$(echo ${header} | awk '{ print $3 }')
    fi

    build/ruse $1 || exit 1
    ./out
    if [ $? -ne $ret ]
    then
	echo "${RED}FAIL${RS} $1"
        rm ./out
	exit 1
    else
	echo "${GREEN}PASS${RS} $1"
        rm ./out
    fi
}

# test test/simple.ruse
test test/if.ruse
# test test/struct.ruse
# test test/struct2.ruse
# test test/typecheck.ruse
# test test/codegen.ruse
