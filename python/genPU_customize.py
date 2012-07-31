import FWCore.ParameterSet.Config as cms

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
        #clone the whole gen jet part
        newsequence = cloneWithSuffix(process.genJetMET, process, "WithPU")
        process.genJetMETWithPU = cms.Path(newsequence)
        process.schedule.insert( process.schedule.index(process.genParticlesWithPU_step)+1, process.genJetMETWithPU)
        # add the collections done with PU to teh output
        process.RAWSIMoutput.outputCommands.extend(['keep *_*WithPU*_*_*'])
        return(process)

