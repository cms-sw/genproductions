#!/usr/bin/env python

proc_card_template = '''set group_subprocesses Auto
import model RS_bulk_ktilda
generate p p > y j j / g h QTD=2 , ( y > h h , h >  b b~ )
output VBF_RS_bulk_MMASS_WWIDTHpc -nojpeg
'''

customizecard_template = '''set param_card mass 39 MASS
set param_card DECAY 39 WIDTH
'''

masses = [1500, 2000, 2500, 3000]
widths = [1, 5, 10, 20, 30]

for m in masses:
  for w in widths:

    f_proc_card = open('VBF_RS_bulk_M{0}_W{1}pc_proc_card.dat'.format(m, w), 'w')
    proc_card = proc_card_template.replace('MASS', str(m))
    proc_card = proc_card.replace('WIDTH', str(w))
    f_proc_card.write(proc_card)
    f_proc_card.close()

    f_customizecard = open('VBF_RS_bulk_M{0}_W{1}pc_customizecards.dat'.format(m, w), 'w')
    customizecard = customizecard_template.replace('MASS', str(m))
    customizecard = customizecard.replace('WIDTH', str(w*m/100.))
    f_customizecard.write(customizecard)
    f_customizecard.close()


