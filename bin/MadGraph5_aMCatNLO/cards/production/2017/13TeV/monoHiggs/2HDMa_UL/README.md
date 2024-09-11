The datacards for the 2HDMa UL signal production are made following the pre-legacy version for monoHiggs to gamma-gamma analysis. The ```run_card.dat``` was modified to include a code snippet so that the PDF is set automatically, following the recommendations from the GEN conveners. The parameters ```asrwgtflavor``` and ```maxjetflavor``` were manually set to 4 and 5, while PDF was automatically set to 325500 and 325300 (LHAPDF ID), respectively for gg and bb production modes. 

```Multigrid_producer_gammagamma.py``` was copied from ```Multigrid_producer.py``` and modifications were made to suit our purpose. The parameters are read from the text file ```par_scans_2HDMa_gammagamma.txt```. The script ```gridpack_generation.sh``` was modified to set the recommended slc and CMSSW version for UL, slc7_amd64_gcc10 and CMSSW_10_6_32. To generate the gridpacks, the bools ```bbProcess``` and ```ggProcess``` were toggled in ```Multigrid_producer_gammagamma.py``` according to the desired production mode.

The setup was run under the ```cmssw-el7``` singularity container under lxplus9. 

