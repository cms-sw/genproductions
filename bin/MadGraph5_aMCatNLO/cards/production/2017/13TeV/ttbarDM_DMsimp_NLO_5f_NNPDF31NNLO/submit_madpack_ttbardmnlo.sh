#!/bin/bash
DMMODEL=()
DMSETUP=()
MCHI=()
MPHI=()
BEAMENERGY=()

# define configuration setups 
for IMPHI in 10 50 100 150 200 250 300 350 400 450 500 ; do 
    for IMCHI in 1  0.2  0.3  0.4  0.45 0.49  0.51 0.55 ; do 
	for IDMMODEL in DMsimp_s_spin0 DMsimp_ps_spin0 ; do 
	    for IDECAY in all lept ; do 
		if [ ${IMCHI} != 1 ]  && [ ${IMPHI} == 100 ]; then
		    MCHI+=( `python -c "print int(${IMPHI}*${IMCHI})"` )		    
		elif [ ${IMCHI} != 1 ]  && [ ${IMPHI} != 100 ]; then 
		    continue    
		else
		    MCHI+=( ${IMCHI} )
		fi
		MPHI+=( ${IMPHI} )
		BEAMENERGY+=( 6500 )
		DMMODEL+=( ${IDMMODEL} )
		DECAY+=( ${IDECAY} )
		if [ ${IDECAY} == "lept" ] ; then 
		    DMSETUP+=( ttbarDM__dilepton__${DMMODEL[${#DMMODEL[@]}-1]}__mchi_${MCHI[${#MCHI[@]}-1]}_mphi_${MPHI[${#MPHI[@]}-1]}_gSM_1_gDM_1_${BEAMENERGY[${#BEAMENERGY[@]}-1]}GeV )
		elif [ ${IDECAY} == "all" ] ; then 
		    DMSETUP+=( ttbarDM__inclusive__${DMMODEL[${#DMMODEL[@]}-1]}__mchi_${MCHI[${#MCHI[@]}-1]}_mphi_${MPHI[${#MPHI[@]}-1]}_gSM_1_gDM_1_${BEAMENERGY[${#BEAMENERGY[@]}-1]}GeV )
		else 
		    echo "Unknown decay channel"
		    return 
		fi
		echo "${DMSETUP[${#DMSETUP[@]}-1]}"
	    done
	done
    done
done

# make cards and submit jobs  
NSETUP=`echo "scale=0; ${#DMSETUP[@]} -1 " | bc`
for ISETUP in `seq 0 ${NSETUP}`; do
    # prepare card files 	
    rm -rf addons/cards/${DMSETUP[${ISETUP}]} 
    mkdir -p addons/cards/${DMSETUP[${ISETUP}]}
    for CARD in run_card proc_card madspin_card customizecards extramodels ; do 
	cp -rp addons/cards/${DMMODEL[${ISETUP}]}_template/${CARD}.dat  addons/cards/${DMSETUP[${ISETUP}]}/${DMSETUP[${ISETUP}]}_${CARD}.dat
    done 
    sed -i -e "s|SUBMCHI|${MCHI[${ISETUP}]}|g" addons/cards/${DMSETUP[${ISETUP}]}/*.dat
    sed -i -e "s|SUBMPHI|${MPHI[${ISETUP}]}|g" addons/cards/${DMSETUP[${ISETUP}]}/*.dat
    sed -i -e "s|SUBDMSETUP|${DMSETUP[${ISETUP}]}|g"  addons/cards/${DMSETUP[${ISETUP}]}/*.dat
    sed -i -e "s|SUBBEAMENERGY|${BEAMENERGY[${ISETUP}]}|g"  addons/cards/${DMSETUP[${ISETUP}]}/*.dat
    sed -i -e "s|SUBDECAY|${DECAY[${ISETUP}]}|g"  addons/cards/${DMSETUP[${ISETUP}]}/*.dat
    
    SUBPATH=${PWD}
    if [ ! -f addons/gridpacks/${DMSETUP[${ISETUP}]}_*_tarball.tar.xz ]; then 
	./submit_gridpack_generation.sh 15000 15000 2nd ${DMSETUP[${ISETUP}]} addons/cards/${DMSETUP[${ISETUP}]} 8nh
    fi
    
done	
