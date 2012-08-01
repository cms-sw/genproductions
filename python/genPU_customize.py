import FWCore.ParameterSet.Config as cms

def initJetAreaSequence(process):
  jamedian_pt_jets_value=0.
  eta_max_ghosts=6.
  from RecoJets.JetProducers.AnomalousCellParameters_cfi import AnomalousCellParameters

  from RecoJets.JetProducers.GenJetParameters_cfi import GenJetParameters
  GenJetParameters.doPUFastjet = cms.bool(False)
  GenJetParameters.doAreaFastjet = cms.bool(True)
  GenJetParameters.doRhoFastjet = cms.bool(True)
  GenJetParameters.Active_Area_Repeats = cms.int32(1)
  GenJetParameters.jetPtMin = cms.double(jamedian_pt_jets_value)
  GenJetParameters.Ghost_EtaMax= cms.double(eta_max_ghosts)


  process.kt2GenJetsWithArea = cms.EDProducer("FastjetJetProducer",
      GenJetParameters,
      AnomalousCellParameters,
      jetAlgorithm = cms.string("Kt"),
      rParam       = cms.double(0.2)
  )

  process.kt4GenJetsWithArea=process.kt2GenJetsWithArea.clone(rParam=0.4)
  process.kt8GenJetsWithArea=process.kt2GenJetsWithArea.clone(rParam=0.8)
  process.kt12GenJetsWithArea=process.kt2GenJetsWithArea.clone(rParam=1.2)

  process.GenJetsWithAreaSeq=cms.Sequence(process.kt2GenJetsWithArea+
                                process.kt4GenJetsWithArea+
                                process.kt8GenJetsWithArea+
                                process.kt12GenJetsWithArea) 

def cloneWithSuffix(sequence, process, suffix):
  newSequence = cms.Sequence()
  #poor main way to iterate over a Sequence
  for module in sequence.__str__().split('+'):
    newmodule = getattr(process, module).clone()
    #chenge the input tag
    newmodule.src.setModuleLabel(newmodule.src.getModuleLabel()+suffix)
    #add the new module to the process with fixed name 
    setattr(process, module+suffix, newmodule)
    newSequence = newSequence + getattr(process, module+suffix)
  
  return newSequence

def customiseGenPU(process):
        #do the genParticles on the mixed events
        from PhysicsTools.HepMCCandAlgos.genParticles_cfi import genParticles
        genParticlesWithPU = genParticles.clone()
        genParticlesWithPU.useCrossingFrame = cms.untracked.bool(True)
        genParticlesWithPU.src = cms.untracked.InputTag("mix")  
        setattr(process, "genParticlesWithPU", genParticlesWithPU)
        process.genParticlesWithPU_step = cms.Path(process.genParticlesWithPU)
        #insert the genParticles with PU in the schedule right after the mixinf has been done
        process.schedule.insert( process.schedule.index(process.digi2raw_step)+1, process.genParticlesWithPU_step )
        #add the area calculation
        initJetAreaSequence(process)
        process.genJetMET = cms.Sequence(process.genJetMET + process.GenJetsWithAreaSeq)
        #clone the whole gen jet part
        newsequence = cloneWithSuffix(process.genJetMET, process, "WithPU")
        process.genJetMETWithPU = cms.Path(newsequence)
        process.schedule.insert( process.schedule.index(process.genParticlesWithPU_step)+1, process.genJetMETWithPU)
        # add the collections done with PU to teh output
        process.RAWSIMoutput.outputCommands.extend(['keep *_*WithPU*_*_*'])
        process.RAWSIMoutput.outputCommands.extend(['keep *_*WithArea*_*_*'])
        return(process)

