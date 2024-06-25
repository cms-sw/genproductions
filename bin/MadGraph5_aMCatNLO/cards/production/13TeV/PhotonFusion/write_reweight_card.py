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
#couplings['ceBRe'] = [np.linspace(-40,40,101), ('smeft','ceBRe33')]
#couplings['ceBIm'] = [np.linspace(-40,40,101), ('smeftcpv','ceBIm33')]

# yy > WW BSM couplings:
couplings['fm0'] = [np.linspace(-300,300,21), ('anoinputs','4')]
couplings['fm1'] = [np.linspace(-1000,1000,21), ('anoinputs','5')]
couplings['fm2'] = [np.linspace(-40,40,21), ('anoinputs','6')]
couplings['fm3'] = [np.linspace(-200,200,21), ('anoinputs','7')]
couplings['fm4'] = [np.linspace(-150,150,21), ('anoinputs','8')]
couplings['fm5'] = [np.linspace(-300,300,21), ('anoinputs','9')]
couplings['fm7'] = [np.linspace(-2000,2000,21), ('anoinputs','11')]
couplings['ft0'] = [np.linspace(-2.3,2.3,21), ('anoinputs','12')]
couplings['ft1'] = [np.linspace(-30,30,21), ('anoinputs','13')]
couplings['ft2'] = [np.linspace(-10,10,21), ('anoinputs','14')]
couplings['ft3'] = [np.linspace(-10,10,21), ('anoinputs','15')]
couplings['ft4'] = [np.linspace(-2,2,21), ('anoinputs','16')]
couplings['ft5'] = [np.linspace(-0.5,0.5,21), ('anoinputs','17')]
couplings['ft6'] = [np.linspace(-1,1,21), ('anoinputs','18')]
couplings['ft7'] = [np.linspace(-2,2,21), ('anoinputs','19')]

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
        f.write('\n')
      f.write('\n')
print('writes: '+outfile+', move this files to cards/PROCESSNAME/PROCESSNAME_reweight_card.dat')
