import os

masses = [   300,    400,    500,    600,    700,    800,    900,  1000,  1100]
gqs    = [  1.28,    1.2,   1.17,   1.15,   1.14,   1.14,   1.13,  1.13,  1.13]
gtaus  = [0.0084, 0.0159, 0.0256, 0.0374, 0.0513, 0.0674, 0.0856, 0.106, 0.129]
types  = ["FL","LL"]
cards  = ["customizecards","extramodels","proc_card","run_card"]
Prefix = "WPrime_tbOnly_"

for t in types:
    for m, gq, gtau in zip(masses, gqs, gtaus):
        Name = Prefix + "{type}_M{mass}".format(type = t, mass = m)
        Path = "../" + Name
        if not os.path.isdir(Path):
            os.system("mkdir {}".format(Path))
        for c in cards:
            if c == "proc_card":
                InputCard = Prefix + t + "_" + c + ".dat"
            else:
                InputCard = Prefix + c + ".dat"
            OutputCard = Path + "/" + Name + "_" + c + ".dat"
            with open(InputCard) as input:
                with open(OutputCard, "w") as output:
                    output.write(input.read().format(mass = m, couplinggq = gq, couplinggtau = gtau, name = Name))
