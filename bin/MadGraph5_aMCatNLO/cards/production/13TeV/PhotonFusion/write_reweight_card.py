import numpy as np
'''
setup parameters and run `python write_reweight_card.py`
'''
def GetName(x):
# function that converts float X.YZZ.. to string of form: XpY for positive mXpY for negative values
  _str=('m' if (x<0) else '')
  x=np.abs(x)
  return _str+'%dp%d'%( int(x), int(round((x % 1)*10)))


# Introduce range for operators using np.linspace(MIN,MAX,NPOINTS)
couplings={}

# yy > tautau BSM couplings:
couplings['ceBRe'] = [np.linspace(-40,40,101), ('smeft','ceBRe33')]
couplings['ceBIm'] = [np.linspace(-40,40,101), ('smeftcpv','ceBIm33')]

# yy > WW BSM couplings:
#couplings['cw'] = [np.linspace(-10,10,21), ('Dim6','2')]
#couplings['cb'] = [np.linspace(-20,20,21), ('Dim6','3')]

#couplings['ceBRe33'] = np.linspace(-10,10,3) # use this line for tests

# output file name
outfile='reweight_card.dat'

# write the card
with open(outfile, 'w') as f:
    f.write('#******************************************************************\n')
    f.write('#                       Reweight Module                           *\n')
    f.write('#******************************************************************\n')
    f.write('change rwgt_dir ./rwgt\n')
    f.write('\n')
    f.write('launch --rwgt_name=dummy\n')
    f.write('\n')
    for sel_coupling in couplings:
      model_params = couplings[sel_coupling][1]
      for val in couplings[sel_coupling][0]:
        f.write('launch --rwgt_name=%s_%s\n'%(sel_coupling,GetName(val)))
        for coupling in couplings:
          if coupling==sel_coupling: f.write('set param_card %s %s %1.5e\n'%(model_params[0],model_params[1],val))
          else: f.write('set param_card %s %s %1.5e\n'%(couplings[coupling][1][0],couplings[coupling][1][1],0))
        f.write('set param_card mass 15 1.777\n')
        f.write('\n')
      f.write('\n')
print('writes: '+outfile+', move this files to cards/PROCESSNAME/PROCESSNAME_reweight_card.dat')
