#!/usr/bin/env python
import os, shutil

def replaceInCard(card, replacements):
  with open(card, 'r') as f:  data = f.read()
  for r in replacements:      data = data.replace(r[0], r[1])
  with open(card, 'w') as f:  f.write(data)

#
# Create heavyNeutrino cards for given parameters
# Returns baseName (useful when this function is called from other scripts) which can be used to find the cards i.e. as baseName/baseName_*.dat
# mass            - mass of the heavy neutrino particle
# coupling        - mixing parameter between the heavy neutrino and lepton
# flavours        - could be e, mu, tau, 2l (e+mu), 3l (e+mu+tau)
# isPre2017       - use older pdf's as used in Moriond17 campaign
# type            - trilepton (n1 --> llnu) or lljj (n1 --> ljj)
# oneFlavourDecay - also limit the decay to the specified flavor
# signFirstFlavor - specify the sign of the first lepton
# noZ             - avoid diagrams containing Z
# Dirac           - use the Dirac model instead of Majorana
#
def makeHeavyNeutrinoCards(mass, coupling, flavours, isPre2017=False, type='trilepton', oneFlavourDecay=False, signFirstFlavor=0, noZ=False, dirac=False):
  if signFirstFlavor==1:  sign = '_l1Plus'
  if signFirstFlavor==-1: sign = '_l1Min'
  else:                   sign = ''

  extraOpt =  ('_Dirac' if dirac else '') + ('_oneFlavorDecay' if oneFlavourDecay else '') + ('_noZ' if noZ else '') + ('_pre2017' if isPre2017 else '') + '_massiveAndCKM' + sign
  baseName = 'HeavyNeutrino_' + type + '_M-' + str(mass) + '_V-' + str(coupling) + '_' + flavours + extraOpt + '_LO'
  model    = 'SM_HeavyN_Dirac_CKM_Masses_LO' if dirac else 'SM_HeavyN_AllMassive_LO'

  try:    os.makedirs(baseName)
  except: pass

  for card in ['madspin_card', 'extramodels', 'run_card', 'proc_card', 'customizecards']:
    try:    shutil.copyfile('templateCards/HeavyNeutrino_template_LO_' + card + '.dat', baseName + '/' + baseName + '_' + card + '.dat')
    except: pass

  replacements = [('MASS',     str(mass)),
                  ('COUPLING', str(coupling)),
                  ('FLAVOURS', flavours),
                  ('TYPE',     type),
                  ('MODEL',    model),
                  ('PDGID',    '9990012' if dirac else '9900012'),
                  ('EXTRA',    extraOpt)]


  if flavours == '2l':    replacements += [('l+ = e+ mu+ ta+', 'l+ = e+ mu+'), ('l- = e- mu- ta-', 'l- = e- mu-')]
  elif flavours == 'e':   replacements += [('l+ = e+ mu+ ta+', 'l+ = e+'),     ('l- = e- mu- ta-', 'l- = e-')]
  elif flavours == 'mu':  replacements += [('l+ = e+ mu+ ta+', 'l+ = mu+'),    ('l- = e- mu- ta-', 'l- = mu-')]
  elif flavours == 'tau': replacements += [('l+ = e+ mu+ ta+', 'l+ = ta+'),    ('l- = e- mu- ta-', 'l- = ta-')]

  if signFirstFlavor==1:  replacements += [('l = l+ l-', 'l = l+')]
  if signFirstFlavor==-1: replacements += [('l = l+ l-', 'l = l-')]

  if oneFlavourDecay:
    if flavours in ['2l']:           replacements += [('ldecay+ = e+ mu+ ta+', 'ldecay+ = e+ mu+'), ('ldecay- = e- mu- ta-', 'ldecay- = e- mu-')]
    if flavours in ['e']:            replacements += [('ldecay+ = e+ mu+ ta+', 'ldecay+ = e+'), ('ldecay- = e- mu- ta-', 'ldecay- = e-')]
    if flavours in ['mu']:           replacements += [('ldecay+ = e+ mu+ ta+', 'ldecay+ = mu+'), ('ldecay- = e- mu- ta-', 'ldecay- = mu-')]
  if flavours in ['3l', '2l', 'e']:  replacements += [('set param_card numixing 1 0.000000e+00', 'set param_card numixing 1 %E' % coupling)]
  if flavours in ['3l', '2l', 'mu']: replacements += [('set param_card numixing 4 0.000000e+00', 'set param_card numixing 4 %E' % coupling)]
  if flavours in ['3l', 'tau']:      replacements += [('set param_card numixing 7 0.000000e+00', 'set param_card numixing 7 %E' % coupling)]

  if isPre2017:
    replacements += [('$DEFAULT_PDF_SETS', '292200')]
    replacements += [('$DEFAULT_PDF_MEMBERS', '292201  =  PDF_set_min\n292302  =  PDF_set_max\nTrue')]

  if type=='lljj':
    replacements += [('n1 > ldecay ldecay v', 'n1 > ldecay j j')]
  if noZ:
    replacements += [('n1 > ldecay ldecay v', 'n1 > ldecay ldecay v / Z')]
    replacements += [('n1 > ldecay j j',      'n1 > ldecay j j / Z')]


  replaceInCard(baseName + '/' + baseName + '_run_card.dat',       replacements)
  replaceInCard(baseName + '/' + baseName + '_proc_card.dat',      replacements)
  replaceInCard(baseName + '/' + baseName + '_customizecards.dat', replacements)
  try:    replaceInCard(baseName + '/' + baseName + '_madspin_card.dat',   replacements)
  except: pass

  return baseName

#
# Generate all cards for all requested combinations of majorana or dirac neutrino, mass and mixing parameter
#
if __name__ == "__main__":
  import math
  def intOrFloat(str):
    try:    return int(str)
    except: return float(str)

  for dirac in [True, False]:
    for flavor in ['e', 'mu', 'tau']:
      for mass in [1,2,3,4,5,10]:
  
        # List of requested mixing parameters (or the square of the mixing parameter)
        if dirac and flavor=='tau':                                      # Dirac: tau
          useV2 = False
          if intOrFloat(mass) == 1:    vs  = [4.66e-1]
          elif intOrFloat(mass) == 2:  vs  = [5.00e-2]
          elif intOrFloat(mass) == 3:  vs  = [1.35e-2]
          elif intOrFloat(mass) == 4:  vs  = [9.28e-3]
          elif intOrFloat(mass) == 5:  vs  = [9.04e-3]
          elif intOrFloat(mass) == 10: vs  = [1.52e-3]
        elif dirac:                                                      # Dirac: e/mu
          useV2 = True 
          if intOrFloat(mass) == 10:   v2s = [1.15e-6]
          elif intOrFloat(mass) == 8:  v2s = [4.59e-6]
          elif intOrFloat(mass) == 6:  v2s = [8.20e-6]
          elif intOrFloat(mass) == 5:  v2s = [4.23e-6]
          elif intOrFloat(mass) == 4:  v2s = [1.69e-5]
          elif intOrFloat(mass) == 3:  v2s = [1.00e-4]
          elif intOrFloat(mass) == 2:  v2s = [1.23e-3, 2.47e-4]
          elif intOrFloat(mass) == 1:  v2s = [9.02e-2, 1.80e-2]
        elif flavor=='tau':                                              # Majorana: tau
          useV2 = False
          if intOrFloat(mass) == 1:    vs  = [3.29e-1]
          elif intOrFloat(mass) == 2:  vs  = [3.53e-2]
          elif intOrFloat(mass) == 3:  vs  = [9.57e-3]
          elif intOrFloat(mass) == 4:  vs  = [6.56e-3]
          elif intOrFloat(mass) == 5:  vs  = [6.39e-3]
          elif intOrFloat(mass) == 10: vs  = [1.08e-3]
        else:                                                            # Majorana: e/mu
          useV2 = True 
          if intOrFloat(mass) == 10:   v2s = [5.73e-7]
          elif intOrFloat(mass) == 8:  v2s = [2.29e-6]
          elif intOrFloat(mass) == 6:  v2s = [4.10e-6]
          elif intOrFloat(mass) == 5:  v2s = [2.12e-6]
          elif intOrFloat(mass) == 4:  v2s = [8.44e-6]
          elif intOrFloat(mass) == 3:  v2s = [5.01e-5]
          elif intOrFloat(mass) == 2:  v2s = [6.17e-4, 1.23e-4]
          elif intOrFloat(mass) == 1:  v2s = [4.51e-2, 9.02e-3]

        if useV2: couplings = [math.sqrt(v2) for v2 in v2s]
        else:     couplings = vs

        for coupling in couplings:
          makeHeavyNeutrinoCards(mass, coupling, flavor, isPre2017=False, type='trilepton')   # Note: for Moriond17 samples should put isPre2017 to True
