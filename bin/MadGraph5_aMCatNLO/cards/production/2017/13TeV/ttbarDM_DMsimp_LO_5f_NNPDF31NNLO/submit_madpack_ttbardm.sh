#!/bin/bash
DMMODEL=()
DMSETUP=()
MCHI=()
MPHI=()
BEAMENERGY=()

for IMPHI in 10 50 100 150 200 250 300 350 400 450 500 ; do 
    for IMCHI in 1  0.2  0.3  0.4  0.45 0.49  0.51 0.55 ; do 
	for IDMMODEL in DMsimp_LO_s_spin0 DMsimp_LO_ps_spin0 ; do
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

# create directory for log and tmp script handling 
SUBPATH=${PWD}
mkdir -p ${SUBPATH}/logs/. 

# start perparing and submitting jobs 
NSETUP=`echo "scale=0; ${#DMSETUP[@]} -1 " | bc`
for ISETUP in `seq 0 ${NSETUP}`; do
    SUBJOB=${DMSETUP[${ISETUP}]}
    # prepare card files 	
    rm -rf addons/cards/${SUBJOB} 
    mkdir -p addons/cards/${SUBJOB}
    for CARD in run_card proc_card madspin_card customizecards extramodels ; do 
	cp -rp addons/cards/${DMMODEL[${ISETUP}]}_template/${CARD}.dat  addons/cards/${SUBJOB}/${SUBJOB}_${CARD}.dat
    done 
    sed -i -e "s|SUBMCHI|${MCHI[${ISETUP}]}|g" addons/cards/${SUBJOB}/*.dat
    sed -i -e "s|SUBMPHI|${MPHI[${ISETUP}]}|g" addons/cards/${SUBJOB}/*.dat
    sed -i -e "s|SUBDMSETUP|${SUBJOB}|g"  addons/cards/${SUBJOB}/*.dat
    sed -i -e "s|SUBBEAMENERGY|${BEAMENERGY[${ISETUP}]}|g"  addons/cards/${SUBJOB}/*.dat
    sed -i -e "s|SUBDECAY|${DECAY[${ISETUP}]}|g"  addons/cards/${SUBJOB}/*.dat

    cp base_gridpack_generation.sh ${SUBJOB}.sh 
    sed -i -e "s|SUBPATH|${SUBPATH}|g" ${SUBJOB}.sh 
    sed -i -e "s|SUBJOB|${SUBJOB}|g" ${SUBJOB}.sh 
    chmod +x ${SUBJOB}.sh
    mv ${SUBJOB}.sh ${SUBPATH}/logs/.
    
    cp condor.sub ${SUBJOB}.sub 
    sed -i -e "s|SUBPATH|${SUBPATH}|g" ${SUBJOB}.sub 
    sed -i -e "s|SUBJOB|${SUBJOB}|g" ${SUBJOB}.sub
    sed -i -e "s|SUBMEM|6G|g" ${SUBJOB}.sub
    sed -i -e "s|SUBCORE|6|g" ${SUBJOB}.sub
    sed -i -e "s|SUBRUNTIME|86400|g" ${SUBJOB}.sub
    mv ${SUBJOB}.sub ${SUBPATH}/logs/.
    if [ ! -f addons/gridpacks/${SUBJOB}_*_tarball.tar.xz ]; then 
	condor_submit logs/${SUBJOB}.sub
    fi
done	
