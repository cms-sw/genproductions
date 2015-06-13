			**Summary of Generation Method and Explanations:**

Our Aim is to generate samples with pp ->  W+W- -> lnujj keeping the polarization of w's intact for further analysis.

1. generate p p > w+ w- j j QCD = 0, QED = 4.
   
We have used the default settings of MadGraph. The MadGraph version was MG5_aMC_v2_2_3. We used DECAY package for decaying of w bosons.
The motivation for using DECAY has been described later in this file.[a]


2. Using the script lhe_parser_LL.py (or lhe_parser_LT.py or lhe_parser_TT.py depending on polarization) we seperated different polarization state of w's.

3. We have two opposite sign w. One decaying to leptonically and one hadronically. So, We divided each file (3 files corresponding to three polarization state LL, LT, and TT) into two halves using script lhe_parser_splitHalf_1.py (extracts events with even numbers) and lhe_parser_splitHalf_2.py (extracts events with odd numbers).

4. Finally, we merged the two file (one file in which w+ decayed leptonically and another in which w- decayed leptonically) using the code mergeLheFiles.cpp (Ref: https://twiki.cern.ch/twiki/bin/view/CMSPublic/SWGuideSubgroupMC#1_2_Using_pLHE_campaigns )



			**[a] Motivation to use DECAY Package:**

Our signal is longitudinally polarized WW. So, spin information becomes crucial for us. But, MadSpin does not preserve the spin information for the intermediate particles while the DECAY package is preserving this info. We discussed this problem with MadGraph authors and they suggested to use the DECAY package for this. The discussion link is:
https://answers.launchpad.net/mg5amcnlo/+question/257782.

The comparison for the two is also presented in the SMP-VV meeting. Here is the link:
https://indico.cern.ch/event/385528/contribution/2/material/slides/0.pdf

Link of DECAY package:
https://launchpad.net/mg5amcnlo/trunk/1.5.0/+download/MadGraph5_v1.5.14.tar.gz



		**Other Changes made in DECAY package to make it compatible with the latest version of Madgraph:**


We are using DECAY package from the older version of MadGraph (MadGraph5_v1_5_14). This is because MadGraph authors does not support DECAY now. This was not compatible with the latest version of madgraph because of change in format of LHE file. So, we made two minor changes in the DECAY package which are explained here:

1. Blank lines from decay_couplings.f : Blank spaces are now present in the new lhe files. So, for that blank line protection added to the file decay_couplings.f. This is explained in the twiki made by Andrey after discussion with MadGraph Authors:

https://twiki.cern.ch/twiki/bin/view/Main/MadgraphPolarization

2. Format of <init> block in decay.f file: Earlier the two numbers in this block was of 5 digit but now its of 6 digit so we just change the format of reading this block. For this decay.f file is modified. We solved this issue with the help of Tim Cox while having discussion at HN. Here is the link for this discussion.
https://hypernews.cern.ch/HyperNews/CMS/get/progQuestions/340/1/1/1/1.html


Please let us know in case you need further information.
