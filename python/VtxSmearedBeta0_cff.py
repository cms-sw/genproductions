import FWCore.ParameterSet.Config as cms
def customiseBeta(process):

    process.VtxSmeared.Beta = 0.0

    return(process)
