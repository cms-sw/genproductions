import sys
import os

mA = [110,120,130,140,150,160,170,180,190,200,225,250,275,300,350,400,450,500,600,700,800,900,1000]
tb = [5,10,15,20,25,30,35,40,45,50,55,60]


with open('makecards.py','w') as fileout:
	fileout.write("# mass,tanb\n")
	fileout.write("params = (\n")
	for mass in mA:
		for tanb in tb:
			if mass == 1000 and tanb == 60:
				fileout.write("\t("+str(mass)+", "+str(tanb)+")\n")
			else:
				fileout.write("\t("+str(mass)+", "+str(tanb)+"),\n")
	fileout.write(")\n\n")
	fileout.write("""
with open("powheg-fh.in_template") as f:
    template = f.read()

for mass, tanb in params: 
    with open("powheg-fh_M{}_tanb{}.in".format(mass, tanb), "w") as f:
        f.write(template.format(mass=mass, tanb=tanb))
""")	 
