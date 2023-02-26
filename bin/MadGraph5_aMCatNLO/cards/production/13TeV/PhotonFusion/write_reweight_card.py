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
#couplings['ceBRe33'] = np.linspace(-10,10,201)
#couplings['ceBIm33'] = np.linspace(-10,10,201)

# yy >WW BSM couplings:
#couplings['ceBRe33'] = np.linspace(-10,10,201)
#couplings['ceBIm33'] = np.linspace(-10,10,201)

couplings['ceBRe33'] = np.linspace(-10,10,3)
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
    for coupling in couplings:
      for val in couplings[coupling]:
        f.write('launch --rwgt_name=%s_%s\n'%(coupling,GetName(val)))
        f.write('set param_card smeft %s %1.5e\n'%(coupling,val))
        f.write('set param_card mass 15 1.777\n')
        f.write('\n')
      f.write('\n')
print('writes: '+outfile+', move this files to cards/PROCESSNAME/PROCESSNAME_reweight_card.dat')
