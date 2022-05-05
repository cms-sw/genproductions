# README
a repository with instructions and scripts for producing HJJ CP samples

## Powheg input files

    The following input files are used for producing gridpacks:

    SM with bornsuppfact: examples/V2/X0jj_13TeV/X0jj_SM.input 
    
    PS with bornsuppfact: examples/V2/X0jj_13TeV/X0jj_PS.input 
    
    MM with bornsuppfact: examples/V2/X0jj_13TeV/X0jj_MM.input 

    The bornsuppfact ensures that most of the events are produced with additional jets, the generator weight is then adjusted to give the correct distributions.
  

## Producing gridpacks

    Run stage 0 locally:

       python ./run_pwg_condor.py -p 0 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT 

    Run stage 1 on condor. Need to execute this stage 5 times, waiting for the jobs to all finish after each submission:
    
        python ./run_pwg_condor.py -p 1 -x 1 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT -q workday -j 20
        python ./run_pwg_condor.py -p 1 -x 2 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT -q workday -j 20
        python ./run_pwg_condor.py -p 1 -x 3 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT -q workday -j 20
        python ./run_pwg_condor.py -p 1 -x 4 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT -q workday -j 20
        python ./run_pwg_condor.py -p 1 -x 5 -i examples/V2/X0jj_13TeV/X0jj_SM.input -m X0jj -f X0jj_SM_HTT -q workday -j 20
