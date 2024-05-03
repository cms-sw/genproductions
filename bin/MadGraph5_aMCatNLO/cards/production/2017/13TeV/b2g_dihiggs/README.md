# diBosonSamples
This is the script to quickly generate the cards and gridpacks ([twiki](https://twiki.cern.ch/twiki/bin/viewauth/CMS/QuickGuideMadGraph5aMCatNLO#Create_the_gridpacks_for_each_pr)) for diboson resonance model, q q~ > X > h h

### Prerequisites
* Python 2.X
* [MadGraph5_aMCatNLO](https://launchpad.net/mg5amcnlo)

### Install and Test
here is the way to install and do a simple test
after executing python script, gridpacks will be generated in several minutes

```
# Check out genproduction package
git clone git@github.com:cms-sw/genproductions.git
# Produce cards
cd genproductions/bin/MadGraph5_aMCatNLO/
cp cards/production/2017/13TeV/b2g_dihiggs/genGridpack_diboson.py .
cp -r cards/production/2017/13TeV/b2g_dihiggs/cards/* cards/. 
python genGridpack_diboson.py

## input cards are now generated in the cards directory
```

### IMPORTANT!! Modification of runcmsgrid_LO.sh
In order to save the PDF uncertainties of NNPDF 3.1 LO (315200), the shell 
script runcmsgrid_LO.sh must be modified. 

Here is one [example](runcmsgrid_LO.sh). But runcmsgrid_LO.sh may be 
modified in the future. Therefore, please double check the following two changes:

```
#within the 5F PDF block of code, modify the following line
NNPDF31_lo_as_0130.LHgrid 1 ---> NNPDF31_lo_as_0130.LHgrid

#within the 4F PDF block of code, add the following line
NNPDF31_lo_as_0130.LHgrid 

```

### Structure
To generate gridpack, you need to prepare some cards for gridpack
[here](cards/) are the template cards for Radion and BulkGraviton
[python script](genGridpack_diboson.py) copies BulkGraviton and Radion template cards and replace the parameters of the cards
after preparing cards, jobs to generate gridpack will be sent to batch system and it will finished in several minutes 

### How do I use it?
* How to generate large number of samples for different mass points

please change this [line](genGridpack_diboson.py#L15), the gridpack with the mass point in array will be generated

* How to change the width

please change this [line](genGridpack_diboson.py#L16) and this [line](genGridpack_diboson.py#L22-L23), width can vary with resonance mass or be fixed  

* How to generate gridpacks locally?

please change this [line](genGridpack_diboson.py#L7-L8)
set exe = 1, sub = 0

* How to generate gridpack via submission of batch jobs at lxplus

please change this [line](genGridpack_diboson.py#L7-L8)
set sub = 1, exe = 0