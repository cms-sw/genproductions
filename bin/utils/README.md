## usage for *request_fragment_check.py* script:

```
usage: request_fragment_check.py [-h] [--prepid PREPID [PREPID ...]]
                                 [--ticket TICKET]

------------------------------------------------ 
   This script currently checks for the following to give an OK, WARNING, or ERROR

   WARNINGS:
      * [WARNING] if time per event > 150 seconds
      * [WARNING] if CMSSW version is not 10_2 and 9_3 and 7_1
      * [WARNING] total number of events > 100000000
      * [WARNING] powheg+pythia sample contains Pythia8PowhegEmissionVetoSettings 
                  - warning to check whether it is a loop induced process
      * [WARNING] if CP5 tune is used but campaign is not Fall18 or Fall17
      * [WARNING] if Fall18 campaing but no parton shower weights configuration in the fragment
      * [WARNING] At least one of the MG5_aMC@NLO tmpdir patches is missing."
      *                    --> Please check that you use:"
      *                        >=  CMSSW_7_1_32_patch1 in 7_1_X or  
      *                        >= CMSSW_9_3_9_patch1 in 9_3_X or
      *                        >= 10_1_3 in 10_1_X or" 
      *                        >= CMSSW_10_2_0_pre2 in 10_2_X.
      *                        Your request uses "+cmssw+" :
      *                         If you are not using a proper CMSSW version, please switch to that or
      *                         re-create the gridpack using the updated genproductions area

   ERRORS:
      * [ERROR] Memory is not 2300 or 4000 GB"
      * [ERROR] Memory is 2300 GB while number of cores is XX but not = 1
      * [ERROR] Memory is 4000 GB while number of cores is 1 but not = 2,4 or 8
      * [ERROR] Gridpack should have used cvmfs path instead of eos path
      * [ERROR] MG5_aMC@NLO multi-run patch missing in gridpack - please re-create a gridpack"
      *            using updated genproductions area
      * [ERROR] May be wrong fragment: powheg/madgraph/mcatnlo in dataset name but settings in 
                fragment not correct or vice versa"
      * [ERROR] Tune configuration wrong in the fragment"
      * [ERROR] PS weights in config but CMSSW version is not 10_2_3 - please check!"	
      * [ERROR] Parton shower weight configuration not OK in the fragment"

   The script also checks if there is no fragment there is a hadronizer used.

optional arguments:
  -h, --help            show this help message and exit
  --prepid PREPID [PREPID ...]
                        check mcm requests using prepids
  --ticket TICKET       check mcm requests using ticket number
```
