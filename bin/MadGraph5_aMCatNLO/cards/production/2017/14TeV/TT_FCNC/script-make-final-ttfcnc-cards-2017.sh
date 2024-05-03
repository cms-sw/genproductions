#!/bin/bash

function makecards
{
  repo=$1
  carddir=$2
  name=$3
  if [ -z $repo ] || [ -z $carddir ] || [ -z $name ]; then
    echo "Usage: script-compare-cards-repo <refcards> <carddir> <name>"
    exit 1
  fi
  if [ ! -d $carddir ]; then
    echo "Error: no carddir $carddir"
    exit 1
  fi

  carddir=$carddir/$name
  if [ -d $carddir ]; then
    echo "Error: dir $carddir exists"
    exit 1
  fi
  mkdir $carddir

  models=(\
          "zeta_zut" \
          "zeta_zct" \
          "kappa_zut" \
          "kappa_zct" \
          "kappa_aut" \
          "kappa_act" \
          "eta_hut" \
          "eta_hct" \
          )
          
  # determine model
  model=''
  for m in ${models[@]}; do
    if [[ $name = *"${m}"* ]]; then
      model=$m
      break
    fi
  done
  if [ -z $model ]; then
    echo "model cannot be determined from name: $name"
    exit 1
  fi
  echo "determined model: $model"

  # determine decay top sign
  decay=''
  if [[ $name = *"leptonic"* ]]; then
    decay='leptonic'
  elif [[ $name = *"hadronic"* ]]; then
    decay='hadronic'
  else
    echo "decay cannot be determined from name: $name"
    exit 1
  fi
  echo "determined decay: $decay"

  # determine decay top sign
  decayTopSign=''
  if [[ $name = *"aTleptonic"* ]] || [[ $name = *"aThadronic"* ]]; then
    decayTopSign='1'
  else
    decayTopSign='2'
  fi
  if [ -z $decayTopSign ]; then
    echo "decayTopSign cannot be determined from name: $name"
    exit 1
  fi
  echo "determined decayTopSign: $decayTopSign"

  # proc_card.dat
  card1=${repo}/madgraph_card/TT_no_top_decay_LO_proc_card.dat
  card2=${carddir}/${name}_proc_card.dat
  if [ ! -f $card1 ]; then echo "Error: no $card1"; exit 1; fi
  echo "copying $card1 to $card2"
  if [ $flagDisableTTjets -eq 1 ]; then
    cat $card1 | sed -e 's/TT_no_top_decay_LO/'${name}'/' | sed -e "s/add process p p > t t~ j/#add process p p > t t~ j/" > $card2
    #cat $card1 | sed -e 's/TT_no_top_decay_LO/'${name}'/' | grep -v "add process p p > t t~ j j j" > $card2
  else
    cat $card1 | sed -e 's/TT_no_top_decay_LO/'${name}'/' | grep -v "add process p p > t t~ j j j" > $card2
  fi

  # run_card.dat
  card1=${repo}/madgraph_card/TT_no_top_decay_LO_run_card.dat
  card2=${carddir}/${name}_run_card.dat
  if [ ! -f $card1 ]; then echo "Error: no $card1"; exit 1; fi
  echo "copying $card1 to $card2"
  if [ $flagDisableTTjets -eq 1 ]; then
    cat $card1 | sed -e "s/1 =  ickkw/0 =  ickkw/" > $card2
  else
    cat $card1 > $card2
  fi
  if [ $flagSwitch14TeV -eq 1 ]; then
    rm -f tmpfiletmp
    mv $card2 tmpfiletmp
    cat tmpfiletmp | sed -e "s/6500.0	=  ebeam/7000.0	=  ebeam/g" > $card2
  fi

  # customizecards.dat
  card1=${repo}/madgraph_card/TT_no_top_decay_LO_customizecards.dat
  card2=${carddir}/${name}_customizecards.dat
  if [ ! -f $card1 ]; then echo "Error: no $card1"; exit 1; fi
  echo "copying $card1 to $card2"
  cat $card1 > $card2

  # extramodels.dat
  card1=${repo}/madgraph_card/TT_no_top_decay_LO_extramodels.dat
  card2=${carddir}/${name}_extramodels.dat
  if [ ! -f $card1 ]; then echo "Error: no $card1"; exit 1; fi
  echo "copying $card1 to $card2"
  cat $card1 > $card2

  # param_card.dat
  card1=${repo}/madspin_card/${model}_param_card_madspin.dat
  card2=${carddir}/${name}_param_card.dat
  if [ ! -f $card1 ]; then echo "Error: no $card1"; exit 1; fi
  echo "copying $card1 to $card2"
  cat $card1 | sed -e 's/xyi/xyi/' > $card2

  # madspin_card.dat
  card1=${repo}/madspin_card/${model}_T${decay}_madspin_card${decayTopSign}.dat
  card2=${carddir}/${name}_madspin_card.dat
  if [ ! -f $card1 ]; then echo "Error: no $card1"; exit 1; fi
  echo "copying $card1 to $card2"
  cat $card1 | sed -e 's/madspin'${decayTopSign}'/madspin/' | sed -e "s/import cmsgrid_final.lhe/#import cmsgrid_final.lhe/" | sed -e "s/import model TopFCNC/#import model TopFCNC/" > $card2

  #proddir=$4
  if [ $flagSubmitProd -eq 1 ]; then
    #if [ -z $proddir ]; then
    #  echo "Usage: script-compare-cards-repo <refcards> <carddir> <name> <proddir>"
    #  exit 1
    #fi
    #if [ ! -d $proddir ]; then
    #  echo "Error: no proddir $proddir"
    #  exit 1
    #fi
    #cd $proddir
    if [ ! -f gridpack_generation.sh ]; then
      echo "Error: no gridpack_generation.sh, running from wrong dir?"
      exit 1
    fi
    submit -N $name -l h_rt=11:59:00 -l h_vmem=15.9G "time ./gridpack_generation.sh $name $carddir"
  fi

  echo "  -> done"
}

flagSwitch14TeV=1
flagSubmitProd=0
flagDisableTTjets=0

datasets=(\
          "TT_aT2ZJ_Tleptonic_kappa_zut_LO" \
          "TT_T2ZJ_aTleptonic_kappa_zut_LO" \
          "TT_aT2ZJ_Tleptonic_kappa_zct_LO" \
          "TT_T2ZJ_aTleptonic_kappa_zct_LO" \
          "TT_aT2ZJ_Thadronic_kappa_zut_LO" \
          "TT_T2ZJ_aThadronic_kappa_zut_LO" \
          "TT_aT2ZJ_Thadronic_kappa_zct_LO" \
          "TT_T2ZJ_aThadronic_kappa_zct_LO" \
          "TT_aT2ZJ_Tleptonic_zeta_zut_LO" \
          "TT_T2ZJ_aTleptonic_zeta_zut_LO" \
          "TT_aT2ZJ_Tleptonic_zeta_zct_LO" \
          "TT_T2ZJ_aTleptonic_zeta_zct_LO" \
          "TT_aT2ZJ_Thadronic_zeta_zut_LO" \
          "TT_T2ZJ_aThadronic_zeta_zut_LO" \
          "TT_aT2ZJ_Thadronic_zeta_zct_LO" \
          "TT_T2ZJ_aThadronic_zeta_zct_LO" \
          "TT_aT2HJ_Thadronic_eta_hct_LO" \
          "TT_aT2HJ_Thadronic_eta_hut_LO" \
          "TT_aT2HJ_Tleptonic_eta_hct_LO" \
          "TT_aT2HJ_Tleptonic_eta_hut_LO" \
          "TT_T2HJ_aThadronic_eta_hct_LO" \
          "TT_T2HJ_aThadronic_eta_hut_LO" \
          "TT_T2HJ_aTleptonic_eta_hct_LO" \
          "TT_T2HJ_aTleptonic_eta_hut_LO" \
          )

refcardsdir="./refcards"
outdir="./allcards"

for dataset in ${datasets[@]}; do
  makecards ${refcardsdir} ${outdir} ${dataset}
done
