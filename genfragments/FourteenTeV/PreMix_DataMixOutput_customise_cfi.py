import FWCore.ParameterSet.Config as cms

def customise(process):
    process.DigiToRaw.remove(process.castorRawData)
    process.GENRAWoutput.outputCommands.append('keep CrossingFramePlaybackInfoExtended_*_*_*')
    process.GENRAWoutput.outputCommands.append('drop CrossingFramePlaybackInfoExtended_mix_*_*')
    process.GENRAWoutput.outputCommands.append('drop PileupSummaryInfos_addPileupInfo_*_*')
    return(process)
