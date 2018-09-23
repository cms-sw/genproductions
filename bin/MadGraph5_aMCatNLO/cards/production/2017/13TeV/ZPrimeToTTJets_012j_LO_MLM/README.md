# CMS Madgraph cards for Z' to top-antitop samples
Author: Sébastien Brochet <sebastien.brochet@cern.ch>

This folder contains all the Madgraph cards used for the generation of Z' to top-antitop CMS samples, with up to 2 extra partons in the matrix element. 
An helper script is provided in order to easily generate all the cards from a reference point, which is `ZPrimeToTTJets_M500GeV_W5GeV`.

## How to generate the cards?

First, edit the cards for the reference point, `ZPrimeToTTJets_M500GeV_W5GeV`. You can change the PDF, the center-of-mass energy, etc. in the `run_card`. You can also edit the process generated in the `proc_card`. The `madspin` card should not require any change. The content of the `customizecards` file is generated automatically, but **it has to be correct for the reference point**.

Once you're done, execute the script `./generateCards.py`. All the cards for all the samples points are now available in the current directory. You can now proceed with the gridpack generation.

### Previous version of cards
Card used for 2016 production are stored here:
bin/MadGraph5_aMCatNLO/cards/production/pre2017/13TeV/ZPrimeToTTJets_0123j_LO_MLM
 