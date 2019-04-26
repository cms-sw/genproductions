import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.ThirteenTeV.SemiVisibleJets.svjHelper import svjHelper

from collections import OrderedDict
from copy import deepcopy
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

# implementation of recursive loop over any number of dimensions
# creates grid of all possible combinations of parameter values
def varyAll(pos,paramlist,sig,sigs):
    param = paramlist[pos][0]
    vals = paramlist[pos][1]
    for v in vals:
        stmp = sig[:]+[v]
        # check if last param
        if pos+1==len(paramlist):
            sigs.add(tuple(stmp))
        else:
            varyAll(pos+1,paramlist,stmp,sigs)

parser = ArgumentParser(formatter_class=ArgumentDefaultsHelpFormatter)
parser.add_argument("-y","--year", dest="year", type=int, default=2016, help="which year to simulate (specifies generator tune)")
args = parser.parse_args()

# specification of tunes for each year
if args.year==2016:
    tune_loc = "Configuration.Generator.Pythia8CUEP8M1Settings_cfi"
    tune_block = "pythia8CUEP8M1SettingsBlock"
    tune_suff = "TuneCUETP8M1_13TeV_pythia8"
elif args.year==2017 or args.year==2018:
    tune_loc = "Configuration.Generator.MCTunes2017.PythiaCP2Settings_cfi"
    tune_block = "pythia8CP2SettingsBlock"
    tune_suff = "TuneCP2_13TeV_pythia8"
else:
    parser.error("Unknown year: "+str(args.year))

# complete set of parameter values
params = OrderedDict([
    ("mZprime", range(1000,5100,100)),
    ("mDark", [1,5] + range(10,110,10)),
    ("rinv", [float(x)/10 for x in range(0,11,1)]),
    ("alpha", ["peak", "high", "low"]),
])

# set to accumulate all scan points
sigs = set()

# 2D scans vs. rinv
params_rinv = deepcopy(params)
params_rinv["mDark"] = [20]
params_rinv["alpha"] = ["peak"]
varyAll(0,list(params_rinv.iteritems()),[],sigs)

# 2D scans vs. mDark
params_mDark = deepcopy(params)
params_mDark["rinv"] = [0.5]
params_mDark["alpha"] = ["peak"]
varyAll(0,list(params_mDark.iteritems()),[],sigs)

# 2D scans vs. alpha
params_alpha = deepcopy(params)
params_alpha["rinv"] = [0.5]
params_alpha["mDark"] = [20]
varyAll(0,list(params_alpha.iteritems()),[],sigs)

# some info on the scan
print("This scan will contain "+str(len(sigs))+" model points")

# format first part of output config
first_part = """
import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from {0} import * 

generator = cms.EDFilter("Pythia8GeneratorFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    RandomizedParameters = cms.VPSet(),
)
""".format(tune_loc)

# append process parameters for each model point
helper = svjHelper()
points = []
for point in sorted(sigs):
    mZprime = point[0]
    mDark = point[1]
    rinv = point[2]
    alpha = point[3]

    # down-weight rinv=0 b/c all events pass filter
    weight = 1.0
    if rinv==0.0: weight = 0.5
    
    helper.setModel(mZprime,mDark,rinv,alpha)
    
    pdict = {
        'weight': weight,
        'processParameters': helper.getPythiaSettings(),
        'name': helper.getOutName(outpre="SVJ",outsuff=""),
    }
    points.append(pdict)

# format last part of config (loop over all points)
last_part = """
for point in points:
    basePythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock, 
        {0},
        processParameters = cms.vstring(point['processParameters']),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            '{1}',
            'processParameters',
        )
    )

    generator.RandomizedParameters.append(
        cms.PSet(
            ConfigWeight = cms.double(point['weight']),
            ConfigDescription = cms.string(point['name']),
            PythiaParameters = basePythiaParameters,
        ),
    )

darkhadronZ2filter = cms.EDFilter("MCParticleModuloFilter",
    moduleLabel = cms.InputTag('generator','unsmeared'),
    particleIDs = cms.vint32(51,53),
    multipleOf = cms.uint32(4),
    absID = cms.bool(True),
)

darkquarkFilter = cms.EDFilter("MCParticleModuloFilter",
    moduleLabel = cms.InputTag('generator','unsmeared'),
    particleIDs = cms.vint32(4900101),
    multipleOf = cms.uint32(2),
    absID = cms.bool(True),
    min = cms.uint32(2),
    status = cms.int32(23),
)

ProductionFilterSequence = cms.Sequence(generator+darkhadronZ2filter+darkquarkFilter)
""".format(tune_block,tune_block.replace("Block",""))

with open("SVJ_Scan_"+str(args.year)+"_"+tune_suff+"_cff.py",'w') as ofile:
    ofile.write(first_part)
    ofile.write("\npoints = "+str(points)+"\n")
    ofile.write(last_part)
