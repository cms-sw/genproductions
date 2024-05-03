import os
import sys

if not os.path.exists('gridpackCards'):
  os.mkdir('gridpackCards')

maxMass = 0
xqcut = 0.0

for ctau in [1,10,100,1000,10000]:

  if not os.path.exists('gridpackCards/' + str(ctau) + 'cm'):
    os.mkdir('gridpackCards/' + str(ctau) + 'cm')

  for process in ['AMSB_chargino','Higgsino']:

    if process == 'AMSB_chargino': maxMass = 2100
    else : maxMass = 1600

    for mass in range(100,maxMass,100):

      if not os.path.exists('gridpackCards/' + str(ctau) + 'cm/' + process + '_M' + str(mass) + 'GeV_ctau' + str(ctau) + 'cm'):
        os.mkdir('gridpackCards/' + str(ctau) + 'cm/' + process + '_M' + str(mass) + 'GeV_ctau' + str(ctau) + 'cm')

      outputDir = 'gridpackCards/%dcm/%s_M%dGeV_ctau%dcm/' % (ctau,process,mass,ctau)

      # Creating customize cards

      baseCustomizeFile = 'customize_cards/%s_M%dGeV_ctauCTAUcm_customizecards.dat' % (process,mass)

      outputCustomizeFile = outputDir + '%s_M%dGeV_ctau%dcm_customizecards.dat' % (process,mass,ctau)
  
      width = (1.973269e-14 / ctau)

      os.system('sed "s/WIDTH/' + str(width) + '/g" ' + baseCustomizeFile + ' > ' + outputCustomizeFile)

      # Creating run cards

      baseRunFile = 'run_cards/PROCESS_MMASSGeV_ctauCTAUcm_run_card.dat'

      outputRunFile = outputDir + '%s_M%dGeV_ctau%dcm_run_card.dat' % (process,mass,ctau)

      if mass >= 100 and mass <= 400: xqcut = 40.0
      if mass >= 500 and mass <= 900: xqcut = 50.0
      if mass >= 1000 and mass <= 1900: xqcut = 60.0
      if mass == 2000: xqcut = 70.0

      os.system('sed "s/_XQCUT/' + str(xqcut) + '/g" ' + baseRunFile + ' > ' + outputRunFile)

      # Creating process cards

      baseProcessFile = 'process_cards/%s_MMASSGeV_ctauCTAUcm_proc_card.dat' % (process)
                         
      outputProcessFile = outputDir + '%s_M%dGeV_ctau%dcm_proc_card.dat' % (process,mass,ctau)

      filename = '%s_M%dGeV_ctau%dcm' % (process,mass,ctau)

      os.system('sed "s/FILENAME/' + filename + '/g" ' + baseProcessFile + ' > ' + outputProcessFile)
