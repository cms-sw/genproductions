import os
from collections import OrderedDict

file_list = [f for f in os.listdir(".") if f.startswith("xsec") and f.endswith(".log")]

odict = OrderedDict()
for fname in sorted(file_list):
    with open(fname) as f:
        contents = f.read().split("\n")
        try:
            x  = [c for c in contents if "After filter: final cross section" in c][0].split("= ")[-1].split()[0]
            dset = fname.split("xsec_")[-1].split(".")[0]
            odict[dset] = "{:.2f}".format(float(x))	
        except:
	    print("Couldn't parse {}".format(fname))
            
with open('xSections.dat', 'write') as out:
    for k, v in odict.items():
        line = k.ljust(100) + v 
        out.write('{}\n'.format(line))
        print(line)
