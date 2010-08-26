def customise(process):
	## HCAL switches to turn off Zero Suppression
	process.hbhereco.dropZSmarkedPassed = cms.bool(False)
	process.hfreco.dropZSmarkedPassed   = cms.bool(False)
	process.horeco.dropZSmarkedPassed   = cms.bool(False)
    return (process)
