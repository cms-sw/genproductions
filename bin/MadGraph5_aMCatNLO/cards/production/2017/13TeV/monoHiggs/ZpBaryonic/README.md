# Mono-h ZpBaryonic model samples
This is the script to quickly generate the cards and gridpacks ([twiki](https://twiki.cern.ch/twiki/bin/viewauth/CMS/QuickGuideMadGraph5aMCatNLO#Create_the_gridpacks_for_each_pr)) for ZpBaryonic model, p p > h chi chi~ QED=3

### Prerequisites
* Python 2.X
* [MadGraph5_aMCatNLO](https://launchpad.net/mg5amcnlo)

### Install and Test
here is the way to install and do a simple test
after executing python script, gridpacks will be generated in several minutes

```
# Check out genproduction package
git clone git@github.com:cms-sw/genproductions.git genproductions -b mg242legacy
# Produce cards
cd genproductions/bin/MadGraph5_aMCatNLO/
cp cards/production/2017/13TeV/monoHiggs/ZpBaryonic/gridpacks_ZpBaryonic.py .
cp -r cards/production/2017/13TeV/monoHiggs/ZpBaryonic/cards/* cards/. 
python gridpacks_ZpBaryonic.py

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


### How do I use it?
* How to generate large number of samples for different mass points

please change this [line](gridpacks_ZpBaryonic.py#L18-L21), the gridpack with the mass point in array will be generated

* How to generate gridpacks locally?

please change this [line](gridpacks_ZpBaryonic.py#L8-L9)
set exe = 1, sub = 0

* How to generate gridpack via submission of batch jobs at lxplus

please change this [line](gridpacks_ZpBaryonic.py#L8-L9)
set sub = 1, exe = 0