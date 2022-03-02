# M,mass,width,hdamp,tanb
params = (
	(125, 121.861000061, 1.03110003471, 32.6352500153, 30),
)


with open("powheg.input_template") as f:
    template = f.read()

for M, mass, width, hdamp, tanb in params:
    with open("powheg_M{}_tanb{}.input".format(M, tanb), "w") as f:
        f.write(template.format(mass=mass, width=width, hdamp=hdamp))
