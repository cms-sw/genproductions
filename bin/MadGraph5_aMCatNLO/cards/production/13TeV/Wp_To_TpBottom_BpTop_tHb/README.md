# CMS Madgraph cards for W' to B'+t OR T'+b samples
Author: Kevin Nash knash201@gmail.com  (original from Emanuele Usai, different MG model)

modified to fit W' to B'+t OR T'+b production 

## How to generate the cards?

Edit cards in the WpToTpB_conf and WpToBpT_conf folders. You can change the PDF, the center-of-mass energy, etc. in the `run_card`. 
You can also edit the process generated in the `proc_card`. The `madspin` card should not require any change. 
The content of the `customizecards` file is generated automatically apart from the couplings and the mass.


Once you're done, execute the script `./generateCards.py`. All the cards for all the samples points are now available in the current directory. You can now proceed with the gridpack generation.
