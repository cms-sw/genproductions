*Generation Method*:

1. generate p p > w+ w- j j QCD=0
   
   We run this command with default settings of MadGraph. And the MadGraph version was MG5_aMC_v2_2_3

2. Using the script lhe_parser_LL.py (or lhe_parser_LT.py or lhe_parser_TT.py depending on polarization) we seperated different polarization state of w's.

3. We have two opposite sign w. One decaying[1] to leptonically and one hadronically. So, We divided each file (3 files corresponding to three polarization state LL, LT, and TT) into two halves using script lhe_parser_splitHalf_1.py (extracts events with even numbers) and lhe_parser_splitHalf_2.py (extracts events with odd numbers).

4. Finally, we merged the two file (one file in which w+ decayed leptonically and another in which w- decayed leptonically) using the code mergeLheFiles.cpp (Ref: https://twiki.cern.ch/twiki/bin/view/CMSPublic/SWGuideSubgroupMC#1_2_Using_pLHE_campaigns )


*To be quick we generated 10 lhe files using step 1 and then we merged them using step 4.

[1] We decayed particles using DECAY package, Which is present in the older version of MadGraph (MadGraph5_v1_5_14, Motivation to use [a]). This was not compatible with the latest version of madgraph because of change in format of LHE file. Two things changed in the format of madgraph.

First change: Blank spaces in the new lhe files. So, for that blank line protection added to the file decay_couplings.f 
Second change: Format of <init> block. Earlier the two numbers in this block
was of 5 digit but now its of 6 digit so we just change the format of reading
this block. For this decay.f file is modified.


[a] Motivation to use DECAY Package:
Our signal is longitudinally polarized WW. So, spin information becomes
crucial for us. But, MadSpin does not preserve the spin information for the
intermediate particles while the DECAY package is preserving this info. We
discussed this problem with MadGraph authors and they suggested to use the
DECAY package for this. The discussion link is
https://answers.launchpad.net/mg5amcnlo/+question/257782. 

The comparison for the two is also presented in the SMP-VV meeting. Here is
the link:
https://indico.cern.ch/event/385528/contribution/2/material/slides/0.pdf

Link of DECAY package:
https://launchpad.net/mg5amcnlo/trunk/1.5.0/+download/MadGraph5_v1.5.14.tar.gz
