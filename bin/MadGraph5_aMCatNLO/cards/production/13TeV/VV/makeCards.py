import os
import sys

outputname = sys.argv[1]

os.mkdir(outputname)

os.system("cp skeleton/run_card.dat {0}/{1}_run_card.dat".format(outputname,outputname))
os.system("cp skeleton/proc_card.dat {0}/{1}_proc_card.dat".format(outputname,outputname))
os.system("sed -i 's|\[outputname\]|{0}|g' {1}/{2}_proc_card.dat".format(outputname,outputname,outputname))
os.system("cp skeleton/madspin_card.dat {0}/{1}_madspin_card.dat".format(outputname,outputname))

