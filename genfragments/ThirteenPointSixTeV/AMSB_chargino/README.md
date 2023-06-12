## Hadronizers and LHE-GEN-SIM config files for Run 3 EXO DisTrk analysis

This folder contains templates for the creation of hadronizers and LHE-GEN-SIM configuration files for the EXO Disappearing Tracks search for Run 3 (for previous analyses check EXO-12-034, EXO-16-044 and EXO-19-010). They are based on the chargino lifetime given by CTAU and its mass MASS, and are created using templates of hadronizers and particle files (used in the `Exotica_HSCP_SIM_cfi` config) depending on the parameters.

To create the hadronizers just `python3 create_hadronizer_config.py`. This will create a folder `hadronizers` and subfolders for distinct `CTAU` with the given hadronizers.

To create the LHE-GEN-SIM config files just `scram b -j 8` after previous step and `python3 create_hadronizer_config.py 2`. This will create a folder `configs` for campaign Run3Summer2022 (and `configsEE` for the Run3Summer2022EE campaign) and subfolders for distinct `CTAU` with the given config files.

**Please, make sure that the path to the gridpack tarball files are correct in files `AMSB_chargino_M-XXXGeV_CTau-YYYcm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_cff.py` and `Higgsino_M-XXXGeV_CTau-YYYcm_TuneCP5_PSweights_13p6TeV_madgraph5_pythia8_cff.py`**
