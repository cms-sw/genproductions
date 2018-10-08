## usage for *request_fragment_check.py* script:

python request_fragment_check.py [prepid in mcm]

This script is now moved to https://github.com/cms-sw/genproductions/blob/master/bin/utils/request_fragment_check.py further updates will be made there. 

This script currently checks for the following to give an *OK*, *WARNING*, or *ERROR*:

WARNINGS:
* if time per event > 150 seconds
* if CMSSW version is not 10_2 and 9_3 and 7_1
* total number of events > 100000000
* powheg+pythia sample contains Pythia8PowhegEmissionVetoSettings - warning to check whether it is a loop induced process
* if CP5 tune is used but campaign is not Fall18 or Fall17
* if Fall18 campaing but no parton shower weights configuration in the fragment

ERRORS:
* [ERROR] Gridpack should have used cvmfs path instead of eos path
* [ERROR] May be wrong fragment: powheg/madgraph/mcatnlo in dataset name but settings in fragment not correct or vice versa"
* [ERROR] Tune configuration wrong in the fragment"
* [ERROR] Parton shower weight configuretion not OK in the fragment"

The script also checks if there is no fragment there is a hadronizer used.  
