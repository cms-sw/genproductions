# CMS Madgraph cards for Top32 model

references: arXiv:0904.4108, arXiv:1110.1565

Author: Anastasia Karavdina <anastasia.karavdina@desy.de>

This folder contains the Madgraph cards used for the generation of Top32 CMS samples. 
An helper script is provided in order to easily generate all the cards from a reference point, which is `Top32_1000_ggChannel` for gg, `Top32_1000_gaChannel` for ga and `Top32_1000_aaChannel` for aa, where a stands for photon.

## How to generate the cards?

First, edit the cards for the reference point, `Top32_1000_ggChannel`. You can change the PDF, the center-of-mass energy, etc. in the `run_card`. You can also edit the process generated in the `proc_card`. The content of the `customizecards` file is generated automatically, but **it has to be correct for the reference point**.

Once you're done, execute the script `./generateCards_gg.py`. All the cards for all gg points are now available in the current directory. You can now proceed with the gridpack generation.

### Previous version of cards
Card used for 2016 production are stored here:
https://github.com/cms-sw/genproductions/tree/33032da0eba3466b92d9f139afd447ce6193b3b6/bin/MadGraph5_aMCatNLO/cards/production/13TeV/Top32
