# CMS Madgraph cards for Z' to T'+top samples
Author: Emanuele Usai name.surname@cern.ch

modified to fit Z' to T'+top production from the B' single production for reference see original

Modified to include W' to T'b 

## How to generate the cards?

Edit cards in the ZpToTpTop_conf folder. You can change the PDF, the center-of-mass energy, etc. in the `run_card`. 
You can also edit the process generated in the `proc_card`. The `madspin` card should not require any change. 
The content of the `customizecards` file is generated automatically apart from the couplings and the mass.


Once you're done, execute the script `./generateCards.py`. All the cards for all the samples points are now available in the current directory. You can now proceed with the gridpack generation.
