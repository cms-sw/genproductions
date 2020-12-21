#!/bin/tcsh

foreach m (500 1000 2000 3000 4000 5000 6000 7000 8000 9000)
foreach c (f1p0 f0p5 f0p1)

setenv mass ${m}
setenv mass_dec ${m}.0
setenv coup ${c}

if (${c} == f1p0) then
setenv coup_dec 1.0
endif
if (${c} == f0p5) then
setenv coup_dec 0.5
endif
if (${c} == f0p1) then
setenv coup_dec 0.1
endif

cat > QstarToGJ_M${mass}_${coup}_TuneCP5_13TeV_pythia8_cfi.py <<EOF
import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                  comEnergy = cms.double(13000.0),
                  crossSection = cms.untracked.double(1.0),
                  filterEfficiency = cms.untracked.double(1.0),
                  maxEventsToPrint = cms.untracked.int32(0),
                  pythiaHepMCVerbosity = cms.untracked.bool(False),
                  pythiaPylistVerbosity = cms.untracked.int32(0),
                  PythiaParameters = cms.PSet(
		        pythia8CommonSettingsBlock,
                        pythia8CP5SettingsBlock,
                        processParameters = cms.vstring(
                            'ExcitedFermion:ug2uStar = on',
                            'ExcitedFermion:dg2dStar = on',
                            '4000001:m0 = ${mass_dec}',
                            '4000001:onMode = off',
                            '4000001:onIfMatch = 22 1',
                            '4000002:m0 = ${mass_dec}',
                            '4000002:onMode = off',
                            '4000002:onIfMatch = 22 2',
                            'ExcitedFermion:Lambda = ${mass_dec}',
                            'ExcitedFermion:coupFprime = ${coup_dec}',
                            'ExcitedFermion:coupF = ${coup_dec}',
                            'ExcitedFermion:coupFcol = ${coup_dec}'
                            ),
                        parameterSets = cms.vstring(
			    'pythia8CommonSettings',
                            'pythia8CP5Settings',
			    'processParameters',
			    )
                  )
)
ProductionFilterSequence = cms.Sequence(generator)


EOF

end
end
