bbH NLO and LO, 4FS
=====

This process comes in two blocks: ybyt and yb2. The two generated *MUST* be summed to have a physical meaning. The instructions and code come from the authors of [arXiv:1409.5301](http://arxiv.org/abs/1409.5301) who kindly helped setting this up. The latest tarballs are located on [this wiki](https://cp3.irmp.ucl.ac.be/projects/madgraph/wiki/bbH) and are available on cvmfs generator area.

The state of the art as of July 2015 was presented by M. Wiesemann during the [10th LHC Higgs Cross Section Working Group Workshop](http://indico.cern.ch/event/350628/timetable/#20150716.detailed).

The following instructions have been tested on lxplus with MadGraph5_aMC@NLO version 2.3.2.2 and CMSSW_7_1_19

# Instructions

```bash
# Get CMSSW
export SCRAM_ARCH=slc6_amd64_gcc481
cmsrel CMSSW_7_1_19
cd CMSSW_7_1_19/src
cmsenv

# Download and untar MG5 230
wget https://launchpad.net/mg5amcnlo/2.0/2.3.0/+download/MG5_aMC_v2.3.2.2.tar.gz
tar -xvzf MG5_aMC_v2.3.2.2.tar.gz

# Get and untar the bbH tarballs in the MG5 directory
cd MG5_aMC_v2_3_2_2/
wget https://cp3.irmp.ucl.ac.be/projects/madgraph/raw-attachment/wiki/bbH/bbH_4FS_yb2_save_central_v2.3.0.tar.gz
wget https://cp3.irmp.ucl.ac.be/projects/madgraph/raw-attachment/wiki/bbH/bbH_4FS_ybyt_save_central_v2.3.0.tar.gz
tar -xvzf bbH_4FS_yb2_save_central_v2.3.0.tar.gz
tar -xvzf bbH_4FS_ybyt_save_central_v2.3.0.tar.gz

# Build the needed externals: IREGI, StdHEP, CutTools
cd vendor/IREGI/src
make clean && make -j 4
cd ../..
cd StdHEP
make clean && make -j 4
cd ..
cd CutTools
make clean && make
cd ../..
# NB: do check, but after building the previous, the symlinks to the librairies should not be broken in bbH_4FS_ybyt/lib and bbH_4FS_yb2/lib

# Link LHAPDF
cd bbH_4FS_ybyt/lib
ln -s -d ${LHAPDF_DATA_PATH} PDFsets
ln -s ${LHAPDF_DATA_PATH}/../../lib/libLHAPDF.a .
ln -s ${LHAPDF_DATA_PATH}/../../lib/libLHAPDF.so .
cd ../..
cd bbH_4FS_yb2/lib
ln -s -d ${LHAPDF_DATA_PATH} PDFsets
ln -s ${LHAPDF_DATA_PATH}/../../lib/libLHAPDF.a .
ln -s ${LHAPDF_DATA_PATH}/../../lib/libLHAPDF.so .
cd ../..

# Edit / patch the cards
cd Cards
# edit running configuration
sed -i -e 's,auto_update = 7,auto_update = 0,1' amcatnlo_configuration.txt
sed -i -e 's,automatic_html_opening = True,automatic_html_opening = False,1' amcatnlo_configuration.txt
sed -i -e "s,lhapdf = lhapdf-config,lhapdf = ${LHAPDF_DATA_PATH}/../../bin/lhapdf-config,1" amcatnlo_configuration.txt
# edit param_card
sed -i -e 's,5 4.750000e+00,5 4.800000e+00,1' param_card.dat
sed -i -e 's,6 1.730000e+02,6 1.725000e+02,1' param_card.dat
# edit run card


#seed
10000 = nevents

#input/mg5_configuration.txt
bbH_4FS_yb2/Cards/amcatnlo_configuration.txt
/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.1.5-ddibom2/share/LHAPDF/../../bin/lhapdf-config
echo "set lhapdf ${LHAPDF_DATA_PATH}/../../bin/lhapdf-config" >> mgconfigscript
echo "set run_mode 2" >> mgconfigscript
echo "save options" >> mgconfigscript

# FIXME: PARAMETERS SIMILAR TO CMS ONES?

# Run!
cd 
```

# RUN AT LO ?
# Since you also asked for the possibility of running LO with appropriate  inputs, there are three things to be noted:
1) The new recommendation is to keep mb(mb)=4.187 as input also at LO (not changing it to the pole mass as done before for LO runs, this will have some significant effect on the total cross section K factor)
2) Choose proper LO PDF sets in Cards/run_card.dat
3) Change the running (for the scale variations) from 2- to 1-loop: Inside SubProcesses/reweight_xsec.f go to function rwgt_muR_dep_fac and comment line 1104 and 1105 (with "! NLO" at the end) and uncomment the two lines below 1106 and 1107 (with "! LO" at the end)
