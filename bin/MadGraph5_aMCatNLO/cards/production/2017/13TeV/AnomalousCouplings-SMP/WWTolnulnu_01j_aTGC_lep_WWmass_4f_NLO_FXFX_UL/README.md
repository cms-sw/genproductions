Gridpacks production: UL WW aTGC via cmsconnect
======================

### 0 - Set up

Get a CMS connect account: https://twiki.cern.ch/twiki/bin/view/CMSPublic/WorkBookCMSConnect#Sign_up_to_CMS_Connect

    ssh -Y <username>@login-el7.uscms.org -o ServerAliveInterval=240

    cmsrel CMSSW_9_3_16 
    
    cd CMSSW_9_3_16/src/
    
    git clone https://github.com/cms-sw/genproductions.git

    cd genproductions/bin/MadGraph5_aMCatNLO
    

### 1 - Set the singularity for generating gridpacks on slc6

    screen 

    cmssw-slc6-condor 

    
### 2 - Submit the gridpacks (interactively)

    ./gridpack_generation.sh <card_name> <card_dir>

Close the tab


### 3 - Monitoring 

Get the <screen_id>

    screen -ls

For recovering the tab:

    screen -r <screen_id>


### 5 - Command example for WWmass-600to800_4f_W_plus

    ./gridpack_generation.sh WWTolnulnu_01j_aTGC_lep_WWmass-600to800_4f_NLO_FXFX_W_plus WWTolnulnu_01j_aTGC_lep_WWmass-600to800_4f_NLO_FXFX_W_plus

    Same for the others

### 6 - Additional information: 

#### 6.1 - Difference between W_minus and W_plus configurations:

In W_minus prod_card.dat:
   
      generate p p > ell+ vl w- $$ t t~ H QED=3 [QCD] @0
      add process p p > ell+ vl w- j $$ t t~ H QED=3 [QCD] @1

Then the w- bsoon is decayed through the madspin.

Equivalently, for for W_plus prod_card:

      generate p p > ell- vl~ w+ $$ t t~ H QED=3 [QCD] @0
      add process p p > ell- vl~ w+ j $$ t t~ H QED=3 [QCD] @1

And the w+ boson is decayed through the madspin.

The reason why the split is performed is because the memory consumption for the both processes in the same job made impossible to run the gridpack on the machines under slc6.

#### 6.2 - Difference between the mass bins:

The WW mass distribution is split at gen level in order to ensure having enough generated events in the sensitive bins at analysis level (high WW mass regime). 
The selection is done in cuts.h for each mass bin. 

Two Examples: 

WWmass-0to400: https://github.com/fmanteca/genproductions/blob/master/bin/MadGraph5_aMCatNLO/cards/production/2017/13TeV/AnomalousCouplings-SMP/WWTolnulnu_01j_aTGC_lep_WWmass_4f_NLO_FXFX_UL/WWTolnulnu_01j_aTGC_lep_WWmass-0to400_4f_NLO_FXFX_W_plus/WWTolnulnu_01j_aTGC_lep_WWmass-0to400_4f_NLO_FXFX_W_plus_cuts.f#L82-L87

WWmass-600to800: https://github.com/fmanteca/genproductions/blob/master/bin/MadGraph5_aMCatNLO/cards/production/2017/13TeV/AnomalousCouplings-SMP/WWTolnulnu_01j_aTGC_lep_WWmass_4f_NLO_FXFX_UL/WWTolnulnu_01j_aTGC_lep_WWmass-600to800_4f_NLO_FXFX_W_minus/WWTolnulnu_01j_aTGC_lep_WWmass-600to800_4f_NLO_FXFX_W_minus_cuts.f#L82-L92

#### 6.3 - Added the Kirill's pull request https://github.com/cms-sw/genproductions/pull/2710, that allows reweighting in decays under v26x. 