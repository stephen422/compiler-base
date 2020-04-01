#!/usr/bin/env python
import re
import sys
import subprocess

bin = 'build/qc'
text = sys.argv[1]

for line in open(text, 'r'):
    if re.search("\[error:", line):
        print(line)

print('running:')
r = subprocess.run([bin, text], stdout=subprocess.PIPE)
# print(r.stdout)
