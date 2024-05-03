import os

masses = [300,500,700,900,1100]
couplings = [0.15,0.3,0.5,1,1.08]

for mass,coupling in zip(masses,couplings):
    newPath = "../WPrime_tb_M{}".format(mass)
    if not os.path.isdir(newPath):
        os.system("mkdir {}".format(newPath))
    for item in os.listdir("."):
        if ".dat" in item:
            with open(item) as input:
                with open("../WPrime_tb_M{mass}/WPrime_tb_M{mass}_{card}".format(mass=mass,card=item[10:]),"w") as output:
                    output.write(input.read().format(mass=mass,coupling=coupling,name="WPrime_tb_M" + str(mass)))
