# README
a repository with instructions and scripts for producing HJJ CP samples

## Powheg input files

    The following input files are used for producing gridpacks:

    SM without bornsuppfact: examples/V2/X0jj_13TeV/X0jj_SM.input 
    
    PS without bornsuppfact: examples/V2/X0jj_13TeV/X0jj_PS.input 
    
    MM without bornsuppfact: examples/V2/X0jj_13TeV/X0jj_MM.input 

    Note the bornsuppfact option ensures that most of the events are produced with additional jets, the generator weight is then adjusted to give the correct distributions, however, we found that this resulted in a few events with very large weights passing the final selections and as this results in large bbb uncertainties in some cases we decided to remove it
  

## Producing gridpacks

    Run stage 0 locally:

       python ./run_pwg_condor.py -p 0 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_v1 

    Run stage 1 on condor. Need to execute this stage 5 times, waiting for the jobs to all finish after each submission:
    
        python ./run_pwg_condor.py -p 1 -x 1 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_v1 -q workday -j 35
        python ./run_pwg_condor.py -p 1 -x 2 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_v1 -q workday -j 35
        python ./run_pwg_condor.py -p 1 -x 3 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_v1 -q workday -j 35
        python ./run_pwg_condor.py -p 1 -x 4 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_v1 -q workday -j 35
        python ./run_pwg_condor.py -p 1 -x 5 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_v1 -q workday -j 35

    Run stage 2 on condor:

    python ./run_pwg_condor.py -p 2 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_v1 -q workday -j 200

    Run stage 3 on condor:

    python ./run_pwg_condor.py -p 3 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_v1 -q workday -j 10

    Run stage 9 locally:

    python ./run_pwg_condor.py -p 9 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_v1

    Note that for stage 9 in order to keep the gridpacks from being too large we dont use the -k 1 option, and we also modified Templates/createTarBall_template.sh adding the exclude_extra parameter to remove some of the .dat files that are produced during the gridpack generation that aren't needed in the tarball - BUT if you used different number of jobs for each stage and/or a different number of iterations for stage 1 then this will probably result in too-many / too few .dat files being removed so the exclude_extra parameter should be modified accordingly. Specifically, this means changing the following options:
1. "--exclude=pwggridinfo-rmn-xg{1,2,3,4}-*.dat --exclude=pwggridinfo-btl-xg{1,2,3,4}-*.dat" so that all but the last iteration is removed e.g in this case we run 5 iterations so we removed files 1-4
2. "--exclude=pwgfullgrid-000{0,2,3,4,5,6,7,8,9}.dat" so that only the 0001 file is retained (the others are all copies of each other so aren't needed). The number of files to remove is determined by the number of jobs ran for stage 3. Note for some reason the 0001 file is the one that is opened rather than the 0000 file.
