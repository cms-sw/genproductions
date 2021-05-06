#!/bin/bash

SETUP=() 
JETBIN=() ; MLLBIN=() ; HTBIN=()

for iJETBIN in 0 1 2 3 4 01234 ; do 
    for iMLLBIN in 1-10 10-50 50-INF ; do 
        for iHTBIN in 0-INF 0-40 40-70 70-100 100-200 200-400 400-600 600-800 800-1200 1200-2500 2500-INF ; do 
        JETBIN+=( ${iJETBIN} )
        MLLBIN+=( ${iMLLBIN} )
        HTBIN+=( ${iHTBIN} )
        SETUP+=( dyellell_${iJETBIN}j_mll-${iMLLBIN}_HT-${iHTBIN}_5FS_LO_MLM ) 
        done
    done
done

MAINPATH=`pwd -P`
if [ -d ${MAINPATH}/cards ] ; then rm -rf ${MAINPATH}/cards ; fi 

for((iSETUP=0;iSETUP<${#SETUP[*]};iSETUP++)) ; do

    echo "Setup is ${SETUP[${iSETUP}]}"
    #create output directory 
    mkdir -p ${MAINPATH}/cards/${SETUP[${iSETUP}]}
    cd ${MAINPATH}/cards/${SETUP[${iSETUP}]}
    #copy all relevant cards from template 
    for iCARD in proc_card.dat run_card.dat customizecards.dat ; do 
	    cp ${MAINPATH}/templates/${iCARD}  ${SETUP[${iSETUP}]}_${iCARD} 
    done
    # complete process card according to jet multiplicity 
	if [[ "${JETBIN[${iSETUP}]}" == "0" ]] ; then
        echo "generate process p p > ell+ ell- " >> ${SETUP[${iSETUP}]}_proc_card.dat  
	elif [[ "${JETBIN[${iSETUP}]}" == "1" ]] ; then
        echo "generate process p p > ell+ ell- j " >> ${SETUP[${iSETUP}]}_proc_card.dat  
	elif [[ "${JETBIN[${iSETUP}]}" == "2" ]] ; then
        echo "generate process p p > ell+ ell- j j " >> ${SETUP[${iSETUP}]}_proc_card.dat  
	elif [[ "${JETBIN[${iSETUP}]}" == "3" ]] ; then
        echo "generate process p p > ell+ ell- j j j " >> ${SETUP[${iSETUP}]}_proc_card.dat  
	elif [[ "${JETBIN[${iSETUP}]}" == "4" ]] ; then
        echo "generate process p p > ell+ ell- j j j j " >> ${SETUP[${iSETUP}]}_proc_card.dat  
	elif [[ "${JETBIN[${iSETUP}]}" == "01234" ]] ; then
        echo "generate process p p > ell+ ell- @0" >> ${SETUP[${iSETUP}]}_proc_card.dat  
	    echo "add process p p > ell+ ell- j @1" >> ${SETUP[${iSETUP}]}_proc_card.dat  
	    echo "add process p p > ell+ ell- j j @2" >> ${SETUP[${iSETUP}]}_proc_card.dat  
        echo "add process p p > ell+ ell- j j j @3" >> ${SETUP[${iSETUP}]}_proc_card.dat  
        echo "add process p p > ell+ ell- j j j j @4" >> ${SETUP[${iSETUP}]}_proc_card.dat  
    else 
		echo "Unknow Setup: ${JETBIN[${iSETUP}]}" 
    fi
    echo "output ${SETUP[${iSETUP}]} -nojpeg" >> ${SETUP[${iSETUP}]}_proc_card.dat 

    # add cut values to cards

    # mll cuts 
    TMP=($(echo "${MLLBIN[${iSETUP}]}" | tr '-' '\n'))
    MLLMIN=${TMP[0]} ; MLLMAX=${TMP[1]}
    if [[ "${MLLMIN}" =~ "INF" ]] ; then sed -i -e "s|SUBMLLMIN|-1|g" *_run_card.dat ; else sed -i -e "s|SUBMLLMIN|${MLLMIN}|g" *_run_card.dat ; fi 
    if [[ "${MLLMAX}" =~ "INF" ]] ; then sed -i -e "s|SUBMLLMAX|-1|g" *_run_card.dat ; else sed -i -e "s|SUBMLLMAX|${MLLMAX}|g" *_run_card.dat ; fi 

    # ht cuts 
    TMP=($(echo "${HTBIN[${iSETUP}]}" | tr '-' '\n'))
    HTMIN=${TMP[0]} ; HTMAX=${TMP[1]}
    if [[ "${HTMIN}" =~ "INF" ]] ; then sed -i -e "s|SUBHTMIN|-1|g" *_run_card.dat ; else sed -i -e "s|SUBHTMIN|${HTMIN}|g" *_run_card.dat ; fi 
    if [[ "${HTMAX}" =~ "INF" ]] ; then sed -i -e "s|SUBHTMAX|-1|g" *_run_card.dat ; else sed -i -e "s|SUBHTMAX|${HTMAX}|g" *_run_card.dat ; fi 

done

cd ${MAINPATH}
