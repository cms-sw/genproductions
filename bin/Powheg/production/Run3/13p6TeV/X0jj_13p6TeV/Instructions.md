Instructions for producing HJJ CP samples

## Powheg input files

    The following input files are used for producing gridpacks:

    SM without bornsuppfact: examples/V2/X0jj_13p6TeV/X0jj_SM.input 
    
    PS without bornsuppfact: examples/V2/X0jj_13p6TeV/X0jj_PS.input 
    
    MM without bornsuppfact: examples/V2/X0jj_13p6TeV/X0jj_MM.input 

    Note the bornsuppfact option ensures that most of the events are produced with additional jets, the generator weight is then adjusted to give the correct distributions, however, we found that this resulted in a few events with very large weights passing the final selections and as this results in large bbb uncertainties in some cases we decided to remove it
  

## Producing gridpacks

    Run stage 0 locally:

       python ./run_pwg_condor.py -p 0 -i examples/V2/X0jj_13p6TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_13p6TeV_v1 

    Run stage 1 on condor. Need to execute this stage 5 times, waiting for the jobs to all finish after each submission:
    
        python ./run_pwg_condor.py -p 1 -x 1 -i examples/V2/X0jj_13p6TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_13p6TeV_v1 -q workday -j 35
        python ./run_pwg_condor.py -p 1 -x 2 -i examples/V2/X0jj_13p6TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_13p6TeV_v1 -q workday -j 35
        python ./run_pwg_condor.py -p 1 -x 3 -i examples/V2/X0jj_13p6TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_13p6TeV_v1 -q workday -j 35
        python ./run_pwg_condor.py -p 1 -x 4 -i examples/V2/X0jj_13p6TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_13p6TeV_v1 -q workday -j 35
        python ./run_pwg_condor.py -p 1 -x 5 -i examples/V2/X0jj_13p6TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_13p6TeV_v1 -q workday -j 35

    Run stage 2 on condor:

    python ./run_pwg_condor.py -p 2 -i examples/V2/X0jj_13p6TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_13p6TeV_v1 -q workday -j 200

    Run stage 3 on condor:

    python ./run_pwg_condor.py -p 3 -i examples/V2/X0jj_13p6TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_13p6TeV_v1 -q workday -j 10

    Run stage 9 locally:

    python ./run_pwg_condor.py -p 9 -i examples/V2/X0jj_13p6TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT_13p6TeV_v1

    Note that for stage 9 in order to keep the gridpacks from being too large we dont use the -k 1 option, and we also modified Templates/createTarBall_template.sh adding the exclude_extra parameter to remove some of the .dat files that are produced during the gridpack generation that aren't needed in the tarball 

