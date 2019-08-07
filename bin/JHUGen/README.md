1)  Need slc6

2) run python file to create grids submit jobs for grids: 
python install.py  --name VBFoffshell  --card card_vbf.dat --link-mela
--vbf-offshell

3) Run check to see if jobs are done and if yes then make tarball. 

python install.py  --name VBFoffshell  --card card_vbf.dat --link-mela
--vbf-offshell --check-jobs

