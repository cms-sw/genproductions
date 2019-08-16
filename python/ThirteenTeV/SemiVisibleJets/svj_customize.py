import FWCore.ParameterSet.Config as cms

def customise(process):
    # genjet/met settings - treat DM stand-ins as invisible
    _particles = ["genParticlesForJetsNoMuNoNu","genParticlesForJetsNoNu","genCandidatesForMET","genParticlesForMETAllVisible"]
    for _prod in _particles:
        if hasattr(process,_prod):
            getattr(process,_prod).ignoreParticleIDs.extend([51,52,53])

    # miniAOD settings
    _pruned = ["prunedGenParticlesWithStatusOne","prunedGenParticles"]
    for _prod in _pruned:
        if hasattr(process,_prod):
            # keep HV & DM particles
            getattr(process,_prod).select.extend([
                "keep (4900001 <= abs(pdgId) <= 4900991 )",
                "keep (51 <= abs(pdgId) <= 53)",
            ])

    return process
