from card_lib import *

class MassPoint():
    '''
    Fancy wrapper to hold a pair of DM mass and mediator mass.
    Forces the user to not mix them up.
    '''
    def __init__(self, m_dm = 0, m_med = 0):
        self.m_dm = m_dm
        self.m_med = m_med
    def __str__(self):
        return "m_dm = {MDM}, m_med = {MMED}".format(MDM = self.m_dm, MMED = self.m_med )
    def __repr__(self):
        return "MassPoint(m_dm={MDM},m_med={MMED})".format(MDM = self.m_dm, MMED = self.m_med )


def get_masses_from_file(mass_file):
    '''
    Read mass points from text file.
    The text file should have one line per DM mass with this formatting:
    MDM : MMED1,MMED2,MMED3,...
    '''
    with open(mass_file,"r") as f:
        lines = [ l for l in f.readlines() if not l.startswith("#") ]

    mass_points = []
    for line in lines:
        parts = line.split(":")
        m_dm = int(parts[0])
        for m_med in [int(x) for x in parts[1].strip("\n").split(",")]:
            mass_points.append(MassPoint(m_dm = m_dm,m_med = m_med))
    return mass_points

class DMCardWriter():
    def __init__(self):
        # Example configuration for MonoZ
        self.name = "Vector_MonoZLL_NLO_Mphi_Mchi_gSM-0p25_gDM-1p0_13TeV-madgraph"
        self.model = "DMsimp_s_spin1"
        self.decay='''
                    define l+ = mu+ e+ ta+
                    define l- = mu- e- ta-
                    decay z > l+ l-
                    '''
        self.process ='''
        generate       p p  > xd xd~ z   [QCD]
        add process    p p  > xd xd~ z j [QCD]
        '''
        self.ptcut_chichi = 49
        self.mass_points = [MassPoint(m_med=1000,m_dm=1),MassPoint(m_med=1000,m_dm=450)]
        self.template_path = "./templates/generic_spin1"
        self.gqa = 0
        self.gqv = 0.25
        self.gdma = 0
        self.gdmv = 1.0

    def get_dataset(self,masspoint):
        dataset = self.name.replace("Mphi","Mphi-{MMED}".format(MMED=masspoint.m_med))
        dataset = dataset.replace("Mchi","Mchi-{MMED}".format(MMED=masspoint.m_dm))
        return dataset

    def create_cards(self):
        ### Produce one set of cards per mass point
        for mp in self.mass_points:
            dataset = self.get_dataset(mp)

            # Create card folder
            outdir = os.path.join("./output",dataset)
            if(not os.path.exists(outdir)):
                os.makedirs(outdir)

            # Copy each of the files from the template directory
            for filebase in os.listdir(self.template_path):
                infile = os.path.join(self.template_path, filebase)

                # File-type dependent treatment
                if(infile.endswith(".dat")):
                    outfile = os.path.join(outdir, "{DATASET}_{FILEBASE}".format(DATASET=dataset, FILEBASE=filebase))
                elif(any( [infile.endswith(x) for x in [".f",".tar.gz",".tgz",".zip"]])):
                    outfile = os.path.join(outdir, "{FILEBASE}".format(FILEBASE=filebase))
                else:
                    continue
                shutil.copyfile(infile, outfile)

                # Fill placeholder values
                if(any( [infile.endswith(x) for x in [".f",".dat"] ])):
                    sed_inplace(outfile,"\@PROCESS",self.process)
                    sed_inplace(outfile,"\@DECAY",self.decay)
                    sed_inplace(outfile,"\@MODEL",self.model)
                    sed_inplace(outfile,"\@DATASET",dataset)
                    sed_inplace(outfile,"\@PTCUT",str(self.ptcut_chichi))
                    sed_inplace(outfile,"\@GQA",str(self.gqa))
                    sed_inplace(outfile,"\@GQV",str(self.gqv))
                    sed_inplace(outfile,"\@GDMA",str(self.gdma))
                    sed_inplace(outfile,"\@GDMV",str(self.gdmv))
                    sed_inplace(outfile,"\@MPHI",str(mp.m_med))
                    sed_inplace(outfile,"\@MCHI",str(mp.m_dm))

