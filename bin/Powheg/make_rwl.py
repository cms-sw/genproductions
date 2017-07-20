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
    fout.write("<weight id='"+str(m_idx)+"'> lhapdf=305800 renscfact="+ \
               m_rensc+" facscfact="+m_facsc+" </weight>\n")
    m_idx = m_idx + 1
		  
fout.write("</weightgroup>\n")

fout.write("<weightgroup name='PDF_variation1' combine='replica' >\n")

# computing weights for NNPDF 3.0 nlo central (for cross-check)
fout.write("<weight id='1501'> lhapdf=292200 </weight>\n")
fout.write("</weightgroup>\n")

m_idx = 2001

fout.write("<weightgroup name='PDF_variation2' combine='hessian' >\n")

# computing weights for 100 NNPDF3.1 nlo variations

for idx in range(305801, 305900) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1
  
# computing weights for NNPDF 3.1 nlo alphas=0.116 variation
# computing weights for NNPDF 3.1 nlo alphas=0.120 variation
fout.write("<weight id='2102'> lhapdf=305901 </weight>\n")
fout.write("<weight id='2103'> lhapdf=305902 </weight>\n")
# computing weights for NNPDF 3.1 lo central
fout.write("<weight id='2104'> lhapdf=315000 </weight>\n")

m_idx = 3001

# computing weights for 56+1 CT14 nlo PDF variations

for idx in range(13100, 13157) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

# computing weights for CT14 nlo alphas=0.116 variation
# computing weights for CT14 nlo alphas=0.120 variation
fout.write("<weight id='3058'> lhapdf=13163 </weight>\n")
fout.write("<weight id='3059'> lhapdf=13167 </weight>\n")
# computing weights for CT14 lo central values
fout.write("<weight id='3060'> lhapdf=13200 </weight>\n")

m_idx = 4001

# computing weights for 50+1 MMHT2014nlo68clas118 PDF variations

for idx in range(25200, 25251) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

# computing weights for 5 MMHT2014nlo68cl 5 alphas variations

#for idx in range(25260, 25265) :
#  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
#  m_idx = m_idx + 1

m_idx = 5001

# computing weights for ABMP16 nlo central value
fout.write("<weight id='4500'> lhapdf=42780 </weight>\n")

# computing weights for PDF4LHC15_100, 100+3 variations
for idx in range(90200, 90302) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 6001

# computing weights for PDF4LHC15_30, 30+3 variations
for idx in range(90400, 90432) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 7001

# computing weights for HERAPDF2.0, 29+14 variations
for idx in range(61100, 61128) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

for idx in range(61130, 61143) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 8001

# computing weights for CT14QED, 30+1 variations
for idx in range(13400, 13430) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

fout.write("</weightgroup>\n")

fout.write("</initrwgt>\n")

fout.close()
