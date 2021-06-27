# mass,tanb
params = (
	(125, 30),
)


with open("powheg-fh.in_template") as f:
    template = f.read()

for mass, tanb in params: 
    with open("powheg-fh_M{}_tanb{}.in".format(mass, tanb), "w") as f:
        f.write(template.format(mass=mass, tanb=tanb))
