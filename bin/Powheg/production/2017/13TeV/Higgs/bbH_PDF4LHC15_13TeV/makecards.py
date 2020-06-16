# mass, width, ncall1
params = (
    (60, 0.004, 600000),
    (80, 0.004, 500000),
    (100, 0.004, 500000),
    (120, 0.004, 500000),
    (125, 0.004, 500000),
    (130, 0.004, 500000),
    (140, 0.004, 500000),
    (160, 0.1, 500000),
    (180, 0.1, 500000),
    (200, 0.1, 500000),
)

with open("bbH_PDF4LHC15_13TeV.input") as f:
    template = f.read()

for mass, width, ncall1 in params:
    hdamp = (mass + 8.36) / 4	# (m(H)+2*m(b)) / 4 
    with open("bbH_PDF4LHC15_13TeV_M{}.input".format(mass), "w") as f:
        f.write(template.format(mass=mass, width=width, hdamp=hdamp, ncall1=ncall1))
