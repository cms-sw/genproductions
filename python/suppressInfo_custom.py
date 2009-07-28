import FWCore.ParameterSet.Config as cms

def customise(process):
	process.MessageLogger.cerr.threshold = 'WARNING'

	return(process)
