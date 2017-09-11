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
    fout.write("<weight id='"+str(m_idx)+"'> lhapdf=306000 renscfact="+ \
               m_rensc+" facscfact="+m_facsc+" </weight>\n")
    m_idx = m_idx + 1
		  
fout.write("</weightgroup>\n")

fout.write("<weightgroup name='PDF_variation1' combine='replica' >\n")

m_idx = 1501
# computing weights for NNPDF 3.0 nlo central (for cross-check)
# fout.write("<weight id='1501'> lhapdf=292200 </weight>\n")
for idx in range(292200, 292202) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1


# computing weights for NNPDF 3.0 nnlo central (for cross-check)
fout.write("<weight id='1701'> lhapdf=292600 </weight>\n")
fout.write("</weightgroup>\n")

m_idx = 2001

fout.write("<weightgroup name='PDF_variation2' combine='hessian' >\n")

# computing weights for 100 NNPDF3.1 nnlo variations
for idx in range(306001, 306101) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1
  
# computing weights for NNPDF 3.1 nnlo alphas=0.116 variation
# computing weights for NNPDF 3.1 nnlo alphas=0.120 variation
fout.write("<weight id='2102'> lhapdf=306101 </weight>\n")
fout.write("<weight id='2103'> lhapdf=306102 </weight>\n")

# computing weights for NNPDF 3.1 nnlo alphas=0.108 variation
fout.write("<weight id='2104'> lhapdf=322500 </weight>\n")
# computing weights for NNPDF 3.1 nnlo alphas=0.110 variation
fout.write("<weight id='2105'> lhapdf=322700 </weight>\n")
# computing weights for NNPDF 3.1 nnlo alphas=0.112 variation
fout.write("<weight id='2106'> lhapdf=322900 </weight>\n")
# computing weights for NNPDF 3.1 nnlo alphas=0.114 variation
fout.write("<weight id='2107'> lhapdf=323100 </weight>\n")
# computing weights for NNPDF 3.1 nnlo alphas=0.117 variation
fout.write("<weight id='2108'> lhapdf=323300 </weight>\n")
# computing weights for NNPDF 3.1 nnlo alphas=0.119 variation
fout.write("<weight id='2109'> lhapdf=323500 </weight>\n")
# computing weights for NNPDF 3.1 nnlo alphas=0.122 variation
fout.write("<weight id='2110'> lhapdf=323700 </weight>\n")
# computing weights for NNPDF 3.1 nnlo alphas=0.124 variation
fout.write("<weight id='2111'> lhapdf=323900 </weight>\n")

m_idx = 3001

# computing weights for 100 NNPDF3.1 nlo variations
for idx in range(305801, 305901) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1
  
# computing weights for NNPDF 3.1 nlo alphas=0.116 variation
# computing weights for NNPDF 3.1 nlo alphas=0.120 variation
fout.write("<weight id='3102'> lhapdf=305901 </weight>\n")
fout.write("<weight id='3103'> lhapdf=305902 </weight>\n")
# computing weights for NNPDF 3.1 lo central
fout.write("<weight id='3104'> lhapdf=315000 </weight>\n")

m_idx = 4001

# computing weights for 56+1 CT14 nlo PDF variations
for idx in range(13100, 13157) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

# computing weights for CT14 nlo alphas=0.116 variation
# computing weights for CT14 nlo alphas=0.120 variation
fout.write("<weight id='4058'> lhapdf=13163 </weight>\n")
fout.write("<weight id='4059'> lhapdf=13167 </weight>\n")
# computing weights for CT14 lo central values
fout.write("<weight id='4060'> lhapdf=13200 </weight>\n")

m_idx = 5001

# computing weights for 56+1 CT14 nnlo PDF variations
for idx in range(13000, 13057) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

# computing weights for CT14 nnlo alphas=0.116 variation
# computing weights for CT14 nnlo alphas=0.120 variation
fout.write("<weight id='5058'> lhapdf=13065 </weight>\n")
fout.write("<weight id='5059'> lhapdf=13069 </weight>\n")

m_idx = 6001

# computing weights for 50+1 MMHT2014nlo68clas118 PDF variations
for idx in range(25200, 25251) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 7001

# computing weights for 50+1 MMHT2014nnlo68cl PDF variations
for idx in range(25300, 25351) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

# computing weights for MMHT2014lo68cl central value
fout.write("<weight id='7060'> lhapdf=25000 </weight>\n")

# computing weights for 5 MMHT2014nlo68cl 5 alphas variations
#for idx in range(25260, 25265) :
#  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
#  m_idx = m_idx + 1

m_idx = 8001

# computing weights for ABMP16 nlo central value
fout.write("<weight id='4500'> lhapdf=42780 </weight>\n")

# computing weights for PDF4LHC15_100_nlo, 100+3 variations
for idx in range(90200, 90303) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 9001

# computing weights for PDF4LHC15_100_nnlo, 100+3 variations
for idx in range(91200, 91303) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 10001

# computing weights for PDF4LHC15_30_nlo, 30+3 variations
for idx in range(90400, 90432) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 11001

# computing weights for PDF4LHC15_30_nnlo, 30+3 variations
for idx in range(91400, 91433) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 12001

# computing weights for HERAPDF2.0_nlo, 29+14 variations
for idx in range(61100, 61129) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

for idx in range(61130, 61144) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 13001

# computing weights for HERAPDF2.0_nnlo, 29+14 variations
for idx in range(61200, 61229) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

for idx in range(61230, 61244) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 14001

# computing weights for CT14QED, 30+1 variations
for idx in range(13400, 13431) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

m_idx = 15001

# computing weights for LUXQED_PDF4LHC_nnlo, 108 variations
for idx in range(82000, 82108) :
  fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
  m_idx = m_idx + 1

fout.write("</weightgroup>\n")

fout.write("</initrwgt>\n")

fout.close()
