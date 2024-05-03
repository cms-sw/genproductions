#!/bin/bash

SETUP=( ) ; CHANNEL=() ; MASS=( ) ; WIDTH=() ; ORDER=( ) ; CP=( ) 

for ICHANNEL in ljets ll incl ; do 
    for IMASS in 400; do
        for IWIDTH in 0.05 ; do 
            for IORDER in RES INT ; do 
	            for ICP in SCALAR PSEUDO ; do 
                    CHANNEL+=( ${ICHANNEL} )
                    MASS+=( ${IMASS} )
	                ORDER+=( ${IORDER} )
	                CP+=( ${ICP} )
	                WIDTHTMP=`echo "scale=0; ${IMASS} * ${IWIDTH}" | bc`
	                WIDTH+=( ${WIDTHTMP} )
	                SETUP+=( heavyhiggs_13TeV_${ICHANNEL}_m${IMASS}_w${WIDTHTMP}_${IORDER}_${ICP} ) 
	            done
            done
        done
    done
done 

NSETUP=`echo "scale=0; ${#SETUP[@]} -1  " | bc`

for ISETUP in `seq 0 ${NSETUP}`; do
    # prepare card files 	
    echo ${SETUP[${ISETUP}]}
    rm -rf ${SETUP[${ISETUP}]} 
    mkdir -p ${SETUP[${ISETUP}]}
    for CARD in customizecards.dat   extramodels.dat   run_card.dat   setscales.f ; do 
	cp -rp templates/${CARD}  ${SETUP[${ISETUP}]}/${SETUP[${ISETUP}]}_${CARD}
    done 
	cp -rp templates/${ORDER[${ISETUP}]}_${CHANNEL[${ISETUP}]}_proc_card.dat  ${SETUP[${ISETUP}]}/${SETUP[${ISETUP}]}_proc_card.dat

    sed -i -e "s|SUBSETUP|${SETUP[${ISETUP}]}|g"  ${SETUP[${ISETUP}]}/*.dat
    sed -i -e "s|SUBMASS|${MASS[${ISETUP}]}|g"  ${SETUP[${ISETUP}]}/*.dat
    sed -i -e "s|SUBWIDTH|${WIDTH[${ISETUP}]}|g"  ${SETUP[${ISETUP}]}/*.dat

    if [ ${ORDER[${ISETUP}]} = 'INT' ]; then
	    if [ ${CP[${ISETUP}]} = 'SCALAR' ]; then
	        sed -i -e "s|SUBCP|a0|g"  ${SETUP[${ISETUP}]}/*.dat
	    elif [ ${CP[${ISETUP}]} = 'PSEUDO' ]; then
	        sed -i -e "s|SUBCP|h0|g"  ${SETUP[${ISETUP}]}/*.dat
	    fi
    elif [ ${ORDER[${ISETUP}]} = 'RES' ];then
        if [ ${CP[${ISETUP}]} = 'SCALAR' ]; then
            sed -i -e "s|SUBCP|h0|g"  ${SETUP[${ISETUP}]}/*.dat
        elif [ ${CP[${ISETUP}]} = 'PSEUDO' ]; then
            sed -i -e "s|SUBCP|a0|g"  ${SETUP[${ISETUP}]}/*.dat
        fi
    fi
    
done	
