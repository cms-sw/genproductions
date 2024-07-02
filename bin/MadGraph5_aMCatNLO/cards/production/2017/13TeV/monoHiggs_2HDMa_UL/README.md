These datacards are copied from pre-legacy 2HDMa with recommended changes to run_card.dat with lhaid set to 325300 (5f for bb production mode) and 325500 (4f for gg production mode). In run_card.dat for gg mode, maxjetflavor is set to 4.

The python script Multigrid_producer.py copies template cards and replaces the parameters for corresponding mass point specified in par_scans_2HDMa.txt file.

Run once for bbProduction and ggProduction modes by setting the bool in Multigrid_producer.py file.

When producing gridpacks, slc7_amd64_gcc10 and CMSSW_10_6_32 were used by changing the gridpack_generation.sh script as per UL recommendation.

NOTE: All commands are run within singularity that wraps CentOS-7 environment on lxplus9. This is done by running command to setup the desired env, and then generate various datacards and gridpacks from within the container.
