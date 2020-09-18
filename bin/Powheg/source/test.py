with open("./compile_report_-_powhegboxV2_rev3728_date20200429_-_slc7_amd64_gcc820_-_CMSSW_11_1_2.log") as f:
#with open("./compile_report_-_powhegboxV2_rev3728_date20200429_-_slc7_amd64_gcc700_-_CMSSW_10_6_9.log") as f:
  lines = f.readlines()

processes = []
results = []
failed = []
a = {}

for line in lines:
  if 'PROCESS' in line or 'Compiling finished' in line or 'Failed to compile' in line:
    #print line.strip()
    if 'PROCESS' in line:
      processes.append(line.split(' ')[-2])
    elif 'Compiling' in line or 'Failed' in line:
      results.append(line.strip())

for i in range(len(processes)):
  a[processes[i]] = results[i]
  if 'Failed' in results[i]:
    failed.append(processes[i])

#print a
print "out of", len(a), "process:"
print processes
print len(failed), "failed"
print failed
