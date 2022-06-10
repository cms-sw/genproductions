#!/usr/bin/env python
import sys
import re

daglog = sys.argv[1]
jobname = sys.argv[2]

print(daglog, jobname)

jobs = []
success = 0

with open(daglog, 'r') as file:
    lines = file.readlines()

print(len(lines))

for i in range(len(lines)):
    if 'Job submitted from host' in lines[i] and jobname in lines[i+1]:
        m = re.match(r'.*\((.*)\).*', lines[i])
        jobs.append(m.group(1))

# print(jobs)

for i in range(len(lines)):
    if 'Job terminated' in lines[i] and 'return value 0' in lines[i+1]:
        m = re.match(r'.*\((.*)\).*', lines[i])
        if m.group(1) in jobs:
            success += 1

success_rate = float(success)/len(jobs)
print(success_rate)

if success_rate > 0.8:
    sys.exit(0)
else:
    sys.exit(1)