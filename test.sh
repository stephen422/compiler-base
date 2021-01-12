#!/bin/sh
test() {
	fail=0
	grep "fail" $1 >/dev/null && fail=1
	build/cmp $1
	if [ $? -ne $fail ]
	then
		echo FAIL $1
		exit 1
	else
		echo PASS $1
	fi
}

test test/struct.ruse

