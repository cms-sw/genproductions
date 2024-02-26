These datacards are copied from pre-legacy with recommended changes to run_card.dat with lhaid set to 325500.

The python script make_datacards_gridpacks.py copies template cards and replaces the parameters for corresponding mass point (by default). 
To generate gridpacks, change exe{n} flag to 1 to run the nth set of mass points described in the script. 

When producing gridpacks, slc7_amd64_gcc10 and CMSSW_10_6_32 were used by changing the gridpack_generation.sh script as per UL recommendation. 

NOTE: All commands are run within singularity that wraps CentOS-7 environment on lxplus9. This is done by running <cmssw-cc7> command to setup the desired env, and then generate various datacards and gridpacks from within the container.
