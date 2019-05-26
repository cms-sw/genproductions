import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.ThirteenTeV.SemiVisibleJets.svjHelper import svjHelper

from collections import OrderedDict
from copy import deepcopy
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import numpy as np

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
parser.add_argument("-n","--num", dest="num", type=int, default=20000, help="number of events for model point w/ weight 1.0 (before filter)")
parser.add_argument("-a","--acc", dest="acc", type=float, default=0.0, help="increase number of events based on acceptance up to this maximum factor")
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
    ("mZprime", range(1500,5200,200)),
    ("mDark", [1,5] + range(10,110,10)),
    ("rinv", [float(x)/10 for x in range(0,11,1)]),
    ("alpha", ["peak", "high", "low"]),
])

# convert named alpha values to numerical
alpha_vals = {
    "peak": -2,
    "high": -1,
    "low": -3,
}
# acceptance values vs. each param
acc = OrderedDict([
    ("mZprime", ([500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000,4100,4200,4300,4400,4500],[4.1e-05,0.00012,0.00012,4.1e-05,0.00027,0.0003,0.00035,0.00033,0.00053,0.0011,0.0014,0.0042,0.0089,0.015,0.023,0.031,0.037,0.047,0.051,0.057,0.061,0.067,0.07,0.074,0.079,0.08,0.081,0.084,0.088,0.089,0.09,0.093,0.093,0.092,0.095,0.098,0.099,0.097,0.098,0.1,0.1])),
    ("mDark", ([1,5,10,20,30,40,50,60,70,80,90,100],[0.084,0.076,0.074,0.08,0.08,0.079,0.08,0.078,0.076,0.076,0.073,0.071])),
    ("rinv", ([0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1],[0.00013,0.03,0.06,0.08,0.089,0.085,0.067,0.042,0.02,0.0054,0.0001])),
    ("alpha", ([-2,-1,-3],[0.08,0.076,0.099])),
])
# acceptance w/ benchmark param values
base_acc = 0.08
# function to use pair of arrays as lookup table
def find_nearest(val,xy):
    x_array = np.asarray(xy[0])
    idx = (np.abs(x_array - val)).argmin()
    return xy[1][idx]
# function to retrieve multiplied relative acceptance
def get_acc(point):
    this_acc = 1.0
    for param,pval in point.iteritems():
        pval = alpha_vals[pval] if param=="alpha" else pval
        this_acc *= find_nearest(pval,acc[param])/base_acc
    return this_acc

# set to accumulate all scan points
sigs = set()

# 2D scans vs. rinv
params_rinv = deepcopy(params)
params_rinv["mDark"] = [20]
params_rinv["alpha"] = ["peak"]
varyAll(0,list(params_rinv.iteritems()),[],sigs)

# 2D scans vs. mDark
params_mDark = deepcopy(params)
params_mDark["rinv"] = [0.3]
params_mDark["alpha"] = ["peak"]
varyAll(0,list(params_mDark.iteritems()),[],sigs)

# 2D scans vs. alpha
params_alpha = deepcopy(params)
params_alpha["rinv"] = [0.3]
params_alpha["mDark"] = [20]
varyAll(0,list(params_alpha.iteritems()),[],sigs)

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
numevents_before = 0
numevents_after = 0
base_filter_eff = 0.5
for point in sorted(sigs):
    mZprime = point[0]
    mDark = point[1]
    rinv = point[2]
    alpha = point[3]

    weight = 1.0
    filter_eff = base_filter_eff
    # down-weight rinv=0 b/c all events pass filter
    if rinv==0.0:
        weight = 0.5
        filter_eff = 1.0
    
    # account for relative acceptance
    if args.acc > 1:
        this_acc = get_acc(OrderedDict([("mZprime",mZprime),("mDark",mDark),("rinv",rinv),("alpha",alpha)]))
        min_weight = weight
        max_weight = weight*args.acc
        weight = np.clip(weight/this_acc,min_weight,max_weight)

    helper.setModel(mZprime,mDark,rinv,alpha)
    
    pdict = {
        'weight': weight,
        'processParameters': helper.getPythiaSettings(),
        'name': helper.getOutName(outpre="SVJ",outsuff=""),
    }
    points.append(pdict)
    numevents_before += args.num*weight
    numevents_after += args.num*weight*filter_eff

# some info on the scan
print("This scan will contain "+str(len(sigs))+" model points, "+str(int(numevents_before))+" events before filter, "+str(int(numevents_after))+" events after filter")

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
