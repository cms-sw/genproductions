## usage for *request_fragment_check.py* script:

(also see https://app.gitbook.com/@monte-carlo-production-tools/s/project/mccontact/request-checking-script)

```
usage: ./request_fragment_check.py [-h] [--prepid PREPID [PREPID ...]]
                                 [--ticket TICKET]

------------------------------------------------ 
   This script currently checks for the following to give an OK, WARNING, or ERROR

   WARNINGS:
                  * [WARNING] comphep, calchep, or herwigpp request. Please check manually
                  * [WARNING] if time per event > 150 seconds
                  * [WARNING] if CMSSW version is not 10_6, 10_2, 9_3, and 7_1 or (8_0 but not Summer16FSPremix) or (9_4 but not Fall17FSPremix) 
                  * [WARNING] total number of events > 100000000
                  * [WARNING] CMSSW version is different than its base UL17 request
                  * [WARNING] No fragment associated to this request"
                  *           is this the hadronizer you intended to use?:
                  * [WARNING] PDF:pSet is missing (if you want to use NNPDF3.1)
                  *           For requests made with >= CMSSW_10_5_0_pre2 and <= CMSSW_10_6_0_patch1
                  *           PDF access method should be like"
                  *           e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'
                  * [WARNING] PDF access method is wrong (if you want to use NNPDF3.1). Please correct:
                  *           e.g. for CP5 use 'PDF:pSet=LHAPDF6:NNPDF31_nnlo_as_0118'"
                  * [WARNING] Dataset name is not regular:
                  *           Please add the Generator name to the dataset.
                  * [WARNING] gridpack patch problem (this is still being investigated)
                  * [WARNING] You may try to request more events per phase-space region in the gridpack.
                  * [WARNING] Didn't find powheg process in runcmsgrid.sh
                  * [WARNING] nJetMax(=X) is not equal to the number of jets specified in the proc card(=Y).
                  *            Is it because this is an exclusive production with additional samples with higher multiplicity generated separately?
                  * [WARNING] To check manually - This is a matched MadGraph LO sample. Please check 'JetMatching:nJetMax' =X is OK and
                  *            correctly set as number of partons in born matrix element for highest multiplicity.
                  * [WARNING] This a MadGraph NLO sample without matching. Please check 'TimeShower:nPartonsInBorn'
                  *                                                   is set correctly as number of coloured particles
                  *                                                  (before resonance decays) in born matrix element.
                  * [WARNING] You're using MG5_aMC vXX in an Ultra Legacy Campaign. Are you sure that's what you want?
                  * [WARNING] nFinal(=X) may not be equal to the number of final state particles before decays (=Y)
                  * [WARNING] To check manually - This is a Powheg NLO sample. Please check 'nFinal' is
                  *               set correctly as number of final state particles (BEFORE THE DECAYS)
                  *                                   in the LHE other than emitted extra parton.
                  * [WARNING] foldphi = X, should be at least 2 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] foldy = X, should be at least 5 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] ncall1 = X, should be at least 550000 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] ncall2 = X, should be at least 75000 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] foldcsi = X, should be at least 2 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] itmx1 = X, should be at least 7 (may be ok if hmass < 150 GeV, please check!)
                  * [WARNING] powheg+pythia sample contains Pythia8PowhegEmissionVetoSettings 
                              - warning to check whether it is a loop induced process
                  * [WARNING] if this is a Powheg request but loop induced process such as gg->ZH,
                  *           then fragment is OK (no need to have Pythia8PowhegEmissionVetoSettings)
                  * [WARNING] Please remove aMCatNLO or POWHEG settings if this is a pure Pythia request.
                  *           If it's not a pure request, in the future, please include madgraph/powheg or amcatnlo
                  *           in the name of the dataset
                  * [WARNING] Check if there is some extra tune setting
                  * [WARNING] No automated check of Sherpa ps/tune parameters yet
                  * [WARNING] Number of extra or replaced tune parameters is at least 
                  *           Please check tune configuration carefully (e.g. are the non-replaced parameters the ones you want)
		  * [WARNING] None standard tune - please check the fragment carefully.
                  * [WARNING] Do you really want to have tune X in this campaign?
                  * [WARNING] No parton shower weights configuration in the fragment. In the Fall18 campaign, 
                              we recommend to include Parton Shower weights
		  * [WARNING] Filters in the fragment but filter efficiency = 1
                  * [WARNING] bwcutoff set to X (> 15 GeV). Note that large bwcutoff values can cause problems in production.
                  * [WARNING] Matched sample but matching efficiency is 1!
                  * [WARNING] Please remove aMCatNLO or POWHEG settings if this is a pure Pythia request.
                  *           If it's not a pure request, in the future, please include madgraph/powheg or amcatnlo
                  *           in the name of the dataset

   ERRORS:
                 * [ERROR] Fragment of XX is different than its base UL17 request: 
                  *         Please make sure that XX has _exactly_ the same settings as XX
                  * [ERROR] missing fragment line(s) for herwig
                  * [ERROR] Herwig7LHEPowhegSettings_cfi should be loaded in the fragment
                  * [ERROR] herwig7LHEPowhegSettingsBlock missing for powheg+herwig7 request"
                  * [ERROR] hw_lhe_Powheg_settings missing for powheg+herwig7 request
                  * [ERROR] Herwig7LHEMG5aMCatNLOSettings_cfi should be loaded in the fragment
                  * [ERROR] herwig7LHEMG5aMCatNLOSettingsBlock missing for MG5_aMC[NLO]+herwig7 request
                  * [ERROR] hw_lhe_MG5aMCatNLO_settings missing for MG5_aMC[NLO]+herwig7 request
                  * [ERROR] please try to increase the filter efficiency if (8*3600/timeperevent)*filter_eff < 50 
                            and timeperevent > 0 and not a ppd request
                  * [ERROR] 8 core request with memory different from 15900 GB. Please set the memory to 15900 GB if CMSSW version >= 10_6_X and nthreads = 8 
                            and mem != 15900 and not ppd request
                  * [ERROR] Memory is not 2300, 4000 or 15900 MB
                  * [ERROR] Memory is 2300 MB while number of cores is XX but not = 1
                  * [ERROR] Memory is 4000 MB while number of cores is 1 but not = 2,4 or 8
                  * [ERROR] Memory is 15900 MB while number of cores is not 8 and 16
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign but Memory is not 14700, 5900, 400, or 2300 MB
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 14700 but nthreads != 8
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 5900 but nthreads != 4
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 4000 but nthreads != 2
                  * [ERROR] HIN-HINPbPbAutumn18GSHIMix or HINPbPbAutumn18wmLHEGSHIMix or HINPbPbAutumn18GS campaign: Memory is 2300 but nthreads != 1
                  * [ERROR] Gridpack should have used cvmfs path instead of eos path
                  * [ERROR] minbias in CMSSW_10_X: SigmaTotal:mode should have been set to 0
                  * [ERROR] minbias in CMSSW_10_X: SigmaTotal:sigmaEl should have been set to 21.89
                  * [ERROR] minbias in CMSSW_10_X: SigmaTotal:sigmaTot should have been set to 100.309
                  * [ERROR] minbias in CMSSW_10_X: PDF access method is wrong. Please correct.
                  * [ERROR] Dataset name does not have the tune name
                  *         Please add the tune name to the dataset.
                  * [ERROR] Dataset name does not contain a parton shower code name
                  *         Please add the parton shower name to the dataset name.
                  * [ERROR] herwigpp = parton_shower not in run_card.dat
                  * [ERROR] Although the name of the dataset has ~Madgraph, the gridpack doesn't seem to be a MG5_aMC one. Please check.
                  * [ERROR] Please add \'set FxFxHandler:MergeMode FxFx\'
	          *         and set FxFxHandler:njetsmax 4
                  * [ERROR] Please load herwig7CommonMergingSettingsBlock
                  * [ERROR] MG5_aMC@NLO multi-run patch missing in gridpack - please re-create a gridpack
                  *            using updated genproductions area
                  * [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing
                  *         And the request is using a version X that does not contain the patch.
                  *         Please use >= 7_1_32_patch1 or CMSSW_9_3_9_patch1 or 10_2_0_pre2
                  * [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing.
                  *         And the request is using a version X that does not contain the patch.
                  *         In this release, please at least use CMSSW_7_1_32_patch1
                  * [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing.
                  *         And the request is using a version X that does not contain the patch.
                  *         In this release, please at least use CMSSW_9_3_9_patch1
                  * [ERROR] At least one of the MG5_aMC@NLO tmpdir patches is missing.
                  *         And the request is using a version X that does not contain the patch.
                  *         In this release, please at least use CMSSW_10_2_0_pre2
                  * [ERROR] didn't find name in powheg.input
                  * [ERROR] MG5_aMC@NLO LO nthreads patch not made in EOS
                  * [ERROR] Fragment may be wrong: check powheg settings in the fragment
                  * [ERROR] You run MG5_aMC@NLO at LO but you have  Pythia8aMCatNLOSettings_cfi in fragment
                  *           --> please remove it from the fragment
                  * [ERROR] Please remove POWHEG settings for MG requests.
                  * [ERROR] You are using a loop induced process, [noborn=QCD].
                  *         Please remove all occurances of Pythia8aMCatNLOSettings from the fragment
                  * [ERROR] You are using a loop induced process, [noborn=QCD].
                  *         Please remove all TimeShower:nPartonsInBorn from the fragment
                  * [ERROR] Tune configuration may be wrong in the fragment
 	    	  *          or pythia8CUEP8M1Settings are overwritten by some other parameters as in CUETP8M2T4
                  * [ERROR] PS weights in config but CMSSW version is < 10_2_3 - please check!
                  * [ERROR] Parton shower weight configuration not OK in the fragment

   The script also checks if there is no fragment there is a hadronizer used.

optional arguments:
  -h, --help            show this help message and exit
  --prepid PREPID [PREPID ...]
                        check mcm requests using prepids
  --ticket TICKET       check mcm requests using ticket number
  --bypass_status       don't check request status in mcm
  --bypass_validation   proceed to next prepid even if there are errors
  --apply_many_threads_patch
                        apply the many threads MG5_aMC@NLO LO patch if
                        necessary
  --dev                 Run on DEV instance of McM
  --debug               Print debugging information

```
