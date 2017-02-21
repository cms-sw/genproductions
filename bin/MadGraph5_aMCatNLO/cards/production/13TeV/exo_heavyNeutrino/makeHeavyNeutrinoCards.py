#!/usr/bin/env python
import os, shutil

def replaceInCard(card, replacements):
  with open(card, 'r') as f:  data = f.read()
  for r in replacements:      data = data.replace(r[0], r[1])
  with open(card, 'w') as f:  f.write(data)

def makeHeavyNeutrinoCards(mass, coupling, flavours):
  baseName = 'HeavyNeutrino_trilepton_M-' + str(mass) + '_V-' + str(coupling) + '_' + flavours + '_NLO'

  try:    os.makedirs(baseName)
  except: pass

  for card in ['madspin_card', 'extramodels', 'run_card', 'proc_card', 'customizecards']:
    shutil.copyfile('templateCards/HeavyNeutrino_trilepton_template_NLO_' + card + '.dat', baseName + '/' + baseName + '_' + card + '.dat')

  replacements = [('MASS', str(mass)), ('COUPLING', str(coupling)), ('FLAVOURS', flavours)]

  if flavours == '2l':    replacements += [('l+ = e+ mu+ ta+', 'l+ = e+ mu+'), ('l- = e- mu- ta-', 'l- = e- mu-')]
  elif flavours == 'e':   replacements += [('l+ = e+ mu+ ta+', 'l+ = e+'),     ('l- = e- mu- ta-', 'l- = e-')]
  elif flavours == 'mu':  replacements += [('l+ = e+ mu+ ta+', 'l+ = mu+'),    ('l- = e- mu- ta-', 'l- = mu-')]
  elif flavours == 'tau': replacements += [('l+ = e+ mu+ ta+', 'l+ = ta+'),    ('l- = e- mu- ta-', 'l- = tau-')]

  if flavours in ['3l', '2l', 'e']:  replacements += [('set param_card numixing 1 0.000000e+00', 'set param_card numixing 1 %E' % coupling)]
  if flavours in ['3l', '2l', 'mu']: replacements += [('set param_card numixing 4 0.000000e+00', 'set param_card numixing 4 %E' % coupling)]
  if flavours in ['3l', 'tau']:      replacements += [('set param_card numixing 7 0.000000e+00', 'set param_card numixing 7 %E' % coupling)]

  replaceInCard(baseName + '/' + baseName + '_proc_card.dat',      replacements)
  replaceInCard(baseName + '/' + baseName + '_customizecards.dat', replacements)


for mass in [1,2,5,10,20,30,40,50,60,80,100,130,150,200,400,600,800,1000]:
  for flavour in ['3l','2l','e','mu','tau']: 
    makeHeavyNeutrinoCards(mass, 0.01, flavour)
