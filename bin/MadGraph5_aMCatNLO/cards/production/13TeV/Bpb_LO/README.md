# CMS Madgraph cards for B' samples
Author: Daniel Gonzalez daniel.gonzalez@cern.ch

modified to fit B' production from the ZPrimeToTTJets for reference see original

## How to generate the cards?

Edit cards in the Bpb_conf folder. You can change the PDF, the center-of-mass energy, etc. in the `run_card`. 
You can also edit the process generated in the `proc_card`. The `madspin` card should not require any change. 
The content of the `customizecards` file is generated automatically apart from the couplings and the mass.


Once you're done, execute the script `./generateCards.py`. All the cards for all the samples points are now available in the current directory. You can now proceed with the gridpack generation.
