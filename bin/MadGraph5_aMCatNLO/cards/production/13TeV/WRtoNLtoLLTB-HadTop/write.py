import os

flavors = {
  "Mu" : ["define ll = mu+ mu-", "define nn = n2"],
  "El" : ["define ll = e+ e-", "define nn = n1"]
}
run = {
  "13TeV" : "6500",
  "13p6TeV" : "6800"
}
masses = {}
count = 0
for i_wr in range(2, 3):# 13):
    m_wr = i_wr*500
    m_n = 300
    masses[str(m_wr)] = []
    while (m_n < m_wr):
        if m_n >= m_wr*0.6:
            masses[str(m_wr)].append(str(m_n))
            count = count + 1
        m_n = m_n + 200
    if m_n-10 > m_wr:
        masses[str(m_wr)].append(str(m_wr-10))
    else:
        masses[str(m_wr)].append(str(m_n-10))

def CopyAtoB(dataset_name, card_name):
    os.system(f"cp base/base_{card_name} cards/{dataset_name}/{dataset_name}_{card_name}")

def SedAtoB(dataset_name, card_name, A, B):
    os.system(f"sed -i 's|@@{A}@@|{B}|g' cards/{dataset_name}/{dataset_name}_{card_name}")

for flavor, defines in flavors.items():
    for com, ebeam in run.items():
        for m_wr, m_ns in masses.items():
            for m_n in m_ns:
                dataset_name = f"WRtoN{flavor}to{flavor}{flavor}TB-HadTop_MWR-{m_wr}_MN-{m_n}_{com}"
                os.system(f"mkdir -p cards/{dataset_name}")
                CopyAtoB(dataset_name, "proc_card.dat")
                CopyAtoB(dataset_name, "run_card.dat")
                CopyAtoB(dataset_name, "customizecards.dat")
                CopyAtoB(dataset_name, "extramodels.dat")
                SedAtoB(dataset_name, "proc_card.dat", "output", dataset_name)
                SedAtoB(dataset_name, "proc_card.dat", "definell", defines[0])
                SedAtoB(dataset_name, "proc_card.dat", "definenn", defines[1])
                SedAtoB(dataset_name, "run_card.dat", "beame", ebeam)
                dummymass = 999999
                dummywidth = 10
                auto = "AUTO"
                if flavor == "El":
                    SedAtoB(dataset_name, "customizecards.dat", "n1mass", m_n)
                    SedAtoB(dataset_name, "customizecards.dat", "n1width", auto)
                    SedAtoB(dataset_name, "customizecards.dat", "n2mass", dummymass)
                    SedAtoB(dataset_name, "customizecards.dat", "n2width", dummywidth)
                else:
                    SedAtoB(dataset_name, "customizecards.dat", "n2mass", m_n)
                    SedAtoB(dataset_name, "customizecards.dat", "n2width", auto)
                    SedAtoB(dataset_name, "customizecards.dat", "n1mass", dummymass)
                    SedAtoB(dataset_name, "customizecards.dat", "n1width", dummywidth)
                SedAtoB(dataset_name, "customizecards.dat", "wrmass", m_wr)
                print (f"./gridpack_generation.sh {dataset_name} cards/{dataset_name}")
