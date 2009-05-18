#!/bin/sh

cd $CMSSW_BASE/src/Configuration/GenProduction/python/
[ ! -f "__init__.py" ] && touch "__init__.py"

PREFIX="PYTHIA6_QCD_Pthat"

(
cat << EOF
15	1.457159248e+09
30	1.090572204e+08
80	1.934639567e+06
170	6.256287713e+04
300	3.664608301e+03
470	3.155131272e+02
800	1.194197450e+01
1400	1.720187180e-01
2200	1.420777800e-03
3000	8.600800000e-06
EOF
) | while read MINCUT XS; do
(
cat << EOF
import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$ Revision: 1.3 $'),
	name = cms.untracked.string('$ Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCD_Pthat.sh,v $'),
	annotation = cms.untracked.string('Summer09: Pythia6 generation of QCD events, 10TeV, D6T tune, pthat > __MINCUT__ GeV')
)

from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
	comEnergy = cms.double(10000.0),
	crossSection = cms.untracked.double(__XS__),
	filterEfficiency = cms.untracked.double(1.0000),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),
	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL=1   ! QCD hight pT processes',
			'CKIN(3)=__MINCUT__  ! minimum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

ProductionFilterSequence = cms.Sequence(generator)
EOF
) | sed -e "s/__MINCUT__/$MINCUT/;s/__XS__/$XS/" > ${PREFIX}_${MINCUT}_10TeV_cff.py

	continue
	CONDITION="FrontierConditions_GlobalTag,IDEAL_31X::All"
	# GEN config files
	cmsDriver.py Configuration/GenProduction/python/${PREFIX}_${MINCUT}_10TeV_cff.py \
		-s GEN:ProductionFilterSequence \
		--mc --eventcontent RAWSIM --datatier GEN \
		--conditions $CONDITION --no_exec -n 1000
	# GEN-HLT config files
	cmsDriver.py Configuration/GenProduction/python/${PREFIX}_${MINCUT}_10TeV_cff.py \
		-s GEN:ProductionFilterSequence,SIM,DIGI,L1,DIGI2RAW,HLT:1E31 \
		--mc --eventcontent RAWSIM --datatier GEN-SIM-RAW \
		--conditions $CONDITION --no_exec -n 10
done
