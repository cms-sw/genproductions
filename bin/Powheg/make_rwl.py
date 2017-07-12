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
    fout.write("<weight id='"+str(m_idx)+"'> lhapdf=260000 renscfact="+ \
               m_rensc+" facscfact="+m_facsc+" </weight>\n")
    m_idx = m_idx + 1
		  
fout.write("</weightgroup>\n")

m_idx = 2001

fout.write("<weightgroup name='PDF_variation1' combine='gaussian' >\n")

# computing weights for 100 NNPDF3.0 nlo variations

for idx in range(260001, 260101) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1
  
# computing weights for NNPDF 3.0 nlo alphas=0.117 variation
# computing weights for NNPDF 3.0 nlo alphas=0.119 variation
fout.write("<weight id='2102'> lhapdf=265000 </weight>\n")
fout.write("<weight id='2103'> lhapdf=266000 </weight>\n")

fout.write("</weightgroup>\n")

m_idx = 3001

# computing weights for 56+1 CT14 nlo PDF variations
fout.write("<weightgroup name='PDF_variation2' combine='hessian' >\n")

for idx in range(13100, 13157) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

# computing weights for CT14 nlo alphas=0.117 variation
# computing weights for CT14 nlo alphas=0.119 variation
fout.write("<weight id='3058'> lhapdf=13164 </weight>\n")
fout.write("<weight id='3059'> lhapdf=13166 </weight>\n")

# computing weights for CT10 nlo central values
fout.write("<weight id='3060'> lhapdf=11000 </weight>\n")

m_idx = 4001

# computing weights for 50+1 MMHT2014nlo68clas118 PDF variations

for idx in range(25200, 25251) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

# computing weights for 5 MMHT2014nlo68cl 5 alphas variations

for idx in range(25260, 25265) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 5001

# computing weights for 56+1 CT14 nnlo PDF variations

for idx in range(13000, 13057) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

fout.write("</weightgroup>\n")

fout.write("</initrwgt>\n")

fout.close()
