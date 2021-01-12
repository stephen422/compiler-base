#!/bin/sh

GREEN="\033[0;32m"
RED="\033[0;31m"
RS="\033[0m"

test() {

	fail=0
	head -n1 $1 | grep "fail" >/dev/null && fail=1
	build/cmp $1
	if [ $? -ne $fail ]
	then
		echo "${RED}FAIL${RS} $1"
		exit 1
	else
		echo "${GREEN}PASS${RS} $1"
	fi
}

test test/struct.ruse
test test/struct2.ruse

