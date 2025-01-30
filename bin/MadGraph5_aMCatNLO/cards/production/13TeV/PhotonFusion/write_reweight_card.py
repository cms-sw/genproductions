import numpy as np
'''
setup parameters and run `python write_reweight_card.py`
'''
def GetName(x):
# function that converts float X.YZZ.. to string of form: XpY for positive mXpY for negative values
  _str=('m' if (x<0) else '')
  x=np.abs(x)
  return _str+'%dp%d'%( int(x), int(round((x % 1)*10)))

# coupling scale (aQGC need to be scaled from TeV-4 to GeV-4)
scale = 1

# Introduce range for operators using np.linspace(MIN,MAX,NPOINTS)
couplings={}

# yy > tautau BSM couplings:
if False:
  couplings['ceBRe'] = [np.linspace(-40,40,101), ('smeft','ceBRe33')]
  couplings['ceBIm'] = [np.linspace(-40,40,101), ('smeftcpv','ceBIm33')]

# yy > WW BSM couplings:
if True:
  scale = 1e-12
  couplings['fm0'] = [np.linspace(-15,15,51), ('aqgc','4')]
  couplings['fm1'] = [np.linspace(-60,60,51), ('aqgc','5')]
  couplings['fm2'] = [np.linspace(-2,2,51), ('aqgc','6')]
  couplings['fm3'] = [np.linspace(-10,10,51), ('aqgc','7')]
  couplings['fm4'] = [np.linspace(-10,10,51), ('aqgc','8')]
  couplings['fm5'] = [np.linspace(-15,15,51), ('aqgc','9')]
  couplings['fm7'] = [np.linspace(-100,100,51), ('aqgc','10')]
  couplings['ft0'] = [np.linspace(-2,2,51), ('aqgc','13')]
  couplings['ft1'] = [np.linspace(-10,10,51), ('aqgc','14')]
  couplings['ft2'] = [np.linspace(-10,10,51), ('aqgc','15')]
  couplings['ft3'] = [np.linspace(-10,10,51), ('aqgc','16')]
  couplings['ft4'] = [np.linspace(-4,4,51), ('aqgc','17')]
  couplings['ft5'] = [np.linspace(-1,1,51), ('aqgc','18')]
  couplings['ft6'] = [np.linspace(-2,2,51), ('aqgc','19')]
  couplings['ft7'] = [np.linspace(-3,3,51), ('aqgc','20')]
  couplings['fm1odd'] = [np.linspace(-2,2,51), ('aqgc','23')]
  couplings['fm2odd'] = [np.linspace(-10,10,51), ('aqgc','24')]
  couplings['fm4odd'] = [np.linspace(-15,15,51), ('aqgc','26')]
  couplings['fm5odd'] = [np.linspace(-10,10,51), ('aqgc','27')]
  couplings['ft2odd'] = [np.linspace(-3,3,51), ('aqgc','30')]
  couplings['ft3odd'] = [np.linspace(-0.5,0.5,51), ('aqgc','31')]
  couplings['ft4odd'] = [np.linspace(-2,2,51), ('aqgc','32')]
  couplings['ft5odd'] = [np.linspace(-1,1,51), ('aqgc','33')]
  couplings['ft6odd'] = [np.linspace(-2,2,51), ('aqgc','34')]

# yy > WW Eboli basis
if False:
  scale = 1e-12
  couplings['fm0'] = [np.linspace(-15,15,51), ('anoinputs','4')]
  couplings['fm1'] = [np.linspace(-60,60,51), ('anoinputs','5')]
  couplings['fm2'] = [np.linspace(-2,2,51), ('anoinputs','6')]
  couplings['fm3'] = [np.linspace(-10,10,51), ('anoinputs','7')]
  couplings['fm4'] = [np.linspace(-10,10,51), ('anoinputs','8')]
  couplings['fm5'] = [np.linspace(-15,15,51), ('anoinputs','9')]
  couplings['fm7'] = [np.linspace(-100,100,51), ('anoinputs','11')]
  couplings['ft0'] = [np.linspace(-2,2,51), ('anoinputs','12')]
  couplings['ft1'] = [np.linspace(-10,10,51), ('anoinputs','13')]
  couplings['ft2'] = [np.linspace(-10,10,51), ('anoinputs','14')]
  couplings['ft3'] = [np.linspace(-10,10,51), ('anoinputs','15')]
  couplings['ft4'] = [np.linspace(-4,4,51), ('anoinputs','16')]
  couplings['ft5'] = [np.linspace(-1,1,51), ('anoinputs','17')]
  couplings['ft6'] = [np.linspace(-2,2,51), ('anoinputs','18')]
  couplings['ft7'] = [np.linspace(-3,3,51), ('anoinputs','19')]

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
          if coupling==sel_coupling: f.write('set param_card %s %s %1.5e\n'%(model_params[0],model_params[1],val*scale))
          else: f.write('set param_card %s %s %1.5e\n'%(couplings[coupling][1][0],couplings[coupling][1][1],0))
        f.write('\n')
      f.write('\n')
print('writes: '+outfile+', move this files to cards/PROCESSNAME/PROCESSNAME_reweight_card.dat')
