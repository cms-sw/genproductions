#!/bin/bash

samples=("WMhadZlepJJ_EWK_LO_SM_mjj100_pTj10" "WMlepWMhadJJ_EWK_LO_SM_mjj100_pTj10" "WMlepZhadJJ_EWK_LO_SM_mjj100_pTj10" "WPhadWMlepJJ_EWK_LO_SM_mjj100_pTj10" "WPhadZlepJJ_EWK_LO_SM_mjj100_pTj10" "WPlepWMhadJJ_EWK_LO_SM_mjj100_pTj10" "WPlepWPhadJJ_EWK_LO_SM_mjj100_pTj10" "WPlepZhadJJ_EWK_LO_SM_mjj100_pTj10" "ZlepZhadJJ_EWK_LO_SM_mjj100_pTj10")

#samples=("WMhadZlepJJ_EWK_LO_QCD_mjj100_pTj10" "WMlepWMhadJJ_EWK_LO_QCD_mjj100_pTj10" "WMlepZhadJJ_EWK_LO_QCD_mjj100_pTj10" "WPhadWMlepJJ_EWK_LO_QCD_mjj100_pTj10" "WPhadZlepJJ_EWK_LO_QCD_mjj100_pTj10" "WPlepWMhadJJ_EWK_LO_QCD_mjj100_pTj10" "WPlepWPhadJJ_EWK_LO_QCD_mjj100_pTj10" "WPlepZhadJJ_EWK_LO_QCD_mjj100_pTj10" "ZlepZhadJJ_EWK_LO_QCD_mjj100_pTj10")

#samples=("aQGC_WMhadZlepJJ_EWK_LO_NPle1_mjj100pt10" "aQGC_WMlepWMhadJJ_EWK_LO_NPle1_mjj100pt10" "aQGC_WMlepZhadJJ_EWK_LO_NPle1_mjj100pt10" "aQGC_WPhadWMlepJJ_EWK_LO_NPle1_mjj100pt10" "aQGC_WPhadZlepJJ_EWK_LO_NPle1_mjj100pt10" "aQGC_WPlepWMhadJJ_EWK_LO_NPle1_mjj100pt10" "aQGC_WPlepWPhadJJ_EWK_LO_NPle1_mjj100pt10" "aQGC_WPlepZhadJJ_EWK_LO_NPle1_mjj100pt10" "aQGC_ZlepZhadJJ_EWK_LO_NPle1_mjj100pt10")

for val in "${samples[@]}"
do
echo $val
cp $val/*_run_card.dat .
sed -i -e "51s/.*/     'lhapdf'    = pdlabel     ! PDF set/"  *run_card.dat
sed -i -e '52s/.*/     263000    = lhaid     ! if pdlabel=lhapdf, this is the lhapdf number/'  *run_card.dat
sed -i -e '282s/.*/NNPDF30_lo_as_0130.LHgrid = sys_pdf # matching scales/' *run_card.dat
mv *_run_card.dat $val/
done

