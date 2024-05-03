# monoH ZpBaryonic samples
This is the script to quickly generate the cards and gridpacks ([twiki](https://twiki.cern.ch/twiki/bin/viewauth/CMS/QuickGuideMadGraph5aMCatNLO#Create_the_gridpacks_for_each_pr)) for ZpBaryonic model, p p > h chi chi~ QED=3

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
cp cards/production/2017/13TeV/monoHiggs/ZpBaryonic/gridpacks_ZpBaryonic.py .
cp cards/production/2017/13TeV/monoHiggs/ZpBaryonic/pdflist_4f_2017.dat ../../MetaData/.
cp -r cards/production/2017/13TeV/monoHiggs/ZpBaryonic/cards/* cards/. 
python gridpacks_ZpBaryonic.py

## input cards are now generated in the cards directory
```

### IMPORTANT!! Modification of PDF list pdflist_4f_2017.dat
In order to save the PDF uncertainties of NNPDF 3.1 LO (315200), the text file 


Here is one [example](pdflist_4f_2017.dat). But pdflist_4f_2017.dat may be 
modified in the future. Therefore, please double check the following change:

```
# add one line at the end
315200 NNPDF31_lo_as_0130 101
```

### Structure
To generate gridpack, you need to prepare some cards for gridpack
[here](cards/) are the template cards for ZpBaryonic. 
[python script](gridpacks_ZpBaryonic.py) copies template cards and replace the parameters of the cards after preparing cards, jobs to generate gridpack will be sent to batch system and it will finished in several minutes 

### How do I use it?

* How to generate gridpacks locally?

please change this [line](gridpacks_ZpBaryonic.py#L5)
set exe = 1

