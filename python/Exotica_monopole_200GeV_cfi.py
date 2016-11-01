import FWCore.ParameterSet.Config as cms

def customise(process):

  process.g4SimHits.Physics.MonopoleMass = 200

  return process
  

