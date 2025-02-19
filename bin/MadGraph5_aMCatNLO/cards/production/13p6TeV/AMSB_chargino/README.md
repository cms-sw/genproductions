## Gridpack cards for Run 3 EXO DisTrk analysis

This folder contains templates for the creation of AMSB gridpack cards for the EXO Disappearing Tracks search for Run 3 (for previous analyses check EXO-12-034, EXO-16-044 and EXO-19-010). They are based on the chargino lifetime given by CTAU, its mass MASS and the type of neutralino referred to as PROCESS, and are created using templates of customize, proc and run cards depending on the parameters.

To create the cards just `python3 createGridpackCards.py` (`python createGridpackCards.py` also works). This will create a folder `gridpackCards`, subfolders for distinct `CTAU`, and subsubfolders containing `PROCESS_MASS_CTAU` for each value of those parameters.
