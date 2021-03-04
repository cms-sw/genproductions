import FWCore.ParameterSet.Config as cms

def customisePU(process):

          process.mix.input.nbPileupEvents.probFunctionVariable=cms.vint32(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)

          process.mix.input.nbPileupEvents.probValue=cms.vdouble(
              0.000374363,0.00019119,0.00130157,0.0238392,0.0828361,
              0.118202,0.119698,0.107855,0.0947511,0.0865411,
              0.0789534,0.0726418,0.067148,0.0575388,0.0425806,
              0.0259079,0.0126324,0.00492452,0.00155633,0.000410135,
              9.32796e-05,1.87681e-05,3.41785e-06,5.98753e-07,1.11116e-07)

          return(process)
      
