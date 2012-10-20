import FWCore.ParameterSet.Config as cms

def monopoles_customise(process):
        pythiapars = process.generator.PythiaParameters.processParameters
        for par in pythiapars:
          if "PMAS(500,1)" in par:
             parsplit=par.split('=')
             mass = parsplit[1].rstrip(' ').rstrip('\n').lstrip(' ')
             break;
        print mass     
        return(process)

