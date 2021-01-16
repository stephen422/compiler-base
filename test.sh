#!/bin/sh

set -e

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

	build/cmp $1
	if [ $? -ne $ret ]
	then
		echo "${RED}FAIL${RS} $1"
		exit 1
	else
		echo "${GREEN}PASS${RS} $1"
	fi
}

test test/struct.ruse
test test/struct2.ruse
test test/typecheck.ruse

