# CMS Madgraph cards for Z'->ttbar+1jet (top-philic) model

references:http://arxiv.org/pdf/1604.07421.pdf, http://arxiv.org/pdf/1410.6099.pdf 

Author: Anastasia Karavdina <anastasia.karavdina@desy.de>

This folder contains all the Madgraph cards used for the generation of Z'->ttbar+1jet (top-philic) CMS samples. 
They are slightly different from the normal Z'->ttbar samples because only the Z' and the jet with Madgraph (including the top
loop in the production) are generated. All decays should be handled with Pythia. 
An helper script is provided in order to easily generate all the cards from a reference point, which is `TopPhilicZprimeToTTbar_M500`.

## How to generate the cards?

First, edit the cards for the reference point, `TopPhilicZprimeToTTbar_M500`. You can change the PDF, the center-of-mass energy, etc. in the `run_card`. You can also edit the process generated in the `proc_card`. The `madspin` card should not require any change. The content of the `customizecards` file is generated automatically, but **it has to be correct for the reference point**.

Once you're done, execute the script `./generateCards.py`. All the cards for all the samples points are now available in the current directory. You can now proceed with the gridpack generation.

### Previous version of cards
Card used for 2016 production are stored here:
bin/MadGraph5_aMCatNLO/cards/production/13TeV/Top_Philic

 