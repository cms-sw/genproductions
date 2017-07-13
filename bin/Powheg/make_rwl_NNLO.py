#!/usr/bin/python

'''
Script for POWHEG weighting calculation
By Yuan CHAO  08/06/2016
'''

import sys
import os

m_outfile = 'pwg-rwl.dat'

m_factor = ['1d0', '2d0', '0.5d0']
m_idx = 1001

fout = open(m_outfile, 'w')

fout.write("<initrwgt>\n")
fout.write("<weightgroup name='scale_variation' combine='envelope' >\n")

for m_rensc in m_factor :
  for m_facsc in m_factor :
    fout.write("<weight id='"+str(m_idx)+"'> lhapdf=303600 renscfact="+ \
               m_rensc+" facscfact="+m_facsc+" </weight>\n")
    m_idx = m_idx + 1
		  
fout.write("</weightgroup>\n")

fout.write("<weightgroup name='PDF_variation1' combine='replica' >\n")

# computing weights for NNPDF 3.0 nnlo central (for cross-check)
fout.write("<weight id='1501'> lhapdf=292600 </weight>\n")
fout.write("</weightgroup>\n")

m_idx = 2001

fout.write("<weightgroup name='PDF_variation2' combine='hessian' >\n")

# computing weights for 100 NNPDF3.1 nnlo variations

for idx in range(303601, 303700) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1
  
# computing weights for NNPDF 3.1 nnlo alphas=0.116 variation
# computing weights for NNPDF 3.1 nnlo alphas=0.120 variation
fout.write("<weight id='2102'> lhapdf=303701 </weight>\n")
fout.write("<weight id='2103'> lhapdf=303702 </weight>\n")
# computing weights for NNPDF 3.1 nlo central
fout.write("<weight id='2104'> lhapdf=305800 </weight>\n")

m_idx = 3001

# computing weights for 56+1 CT14 nnlo PDF variations

for idx in range(13000, 13057) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

# computing weights for CT14 nlo alphas=0.116 variation
# computing weights for CT14 nlo alphas=0.120 variation
fout.write("<weight id='3058'> lhapdf=13065 </weight>\n")
fout.write("<weight id='3059'> lhapdf=13069 </weight>\n")
# computing weights for CT14 nlo central values
fout.write("<weight id='3060'> lhapdf=13100 </weight>\n")

m_idx = 4001

# computing weights for 50+1 MMHT2014nnlo68cl PDF variations

for idx in range(25300, 25351) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

# computing weights for 5 MMHT2014nlo68cl 5 alphas variations

#for idx in range(25360, 25365) :
#  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
#  m_idx = m_idx + 1

m_idx = 5001

# computing weights for ABMP16 nnlo central value
fout.write("<weight id='4500'> lhapdf=42780 </weight>\n")

# computing weights for PDF4LHC15_100, 100+3 variations
for idx in range(91200, 91302) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 6001

# computing weights for PDF4LHC15_30, 30+3 variations
for idx in range(91400, 91432) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

#m_idx = 7001

# computing weights for HERAPDF2.0, 29+14 variations
#for idx in range(61100, 61128) :
#  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
#  m_idx = m_idx + 1

#for idx in range(61130, 61143) :
#  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
#  m_idx = m_idx + 1

#m_idx = 8001

# computing weights for CT14QED, 30+1 variations
#for idx in range(13400, 13430) :
#  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
#  m_idx = m_idx + 1

fout.write("</weightgroup>\n")

fout.write("</initrwgt>\n")

fout.close()
