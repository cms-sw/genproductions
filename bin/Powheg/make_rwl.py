#!/usr/bin/python

'''
Script for POWHEG weighting calculation
By Yuan CHAO  08/06/2016
'''

import sys
import os

if len(sys.argv) < 3:
    print """\
ERROR: Please specify if the Flavor scheme for which you want to define the weights is 5F (1) or 4F (0), and the central PDF
Example of usage for 5F:  python make_rwl.py 1 306000
Example of usage for 4F:  python make_rwl.py 0 320900
"""
    sys.exit(1)

is5FlavorScheme = str(sys.argv[1])
CentralPDF = str(sys.argv[2])

# is5FlavorScheme = True
# CentralPDF = 306000
# processes4Flavor = ['ST_tch_4f', 'bbH', 'Wbb_dec', 'Wbbj',]

# if processName in processes4Flavor:
  # is5FlavorScheme = False
  # CentralPDF = 320900

# print 'INFO: The selected process '+processName+'uses',('5F' if is5FlavorScheme == True else '4F'),'PDF Flavor Scheme, using central PDF set', CentralPDF

m_outfile = 'pwg-rwl.dat'

m_factor = ['1d0', '2d0', '0.5d0']
m_idx = 1001

fout = open(m_outfile, 'w')

fout.write("<initrwgt>\n")
fout.write("<weightgroup name='scale_variation' combine='envelope' >\n")

for m_rensc in m_factor :
  for m_facsc in m_factor :
    fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(CentralPDF)+" renscfact="+ \
               m_rensc+" facscfact="+m_facsc+" </weight>\n")
    m_idx = m_idx + 1
		  
fout.write("</weightgroup>\n")

if int(is5FlavorScheme) == 1:
  # 5F PDF
  pdf_sets = {
            # weight id, LHAPDF id, name, replicas to be written
            "PDF_variation1 , hessian" :
            [
              [2000, 306000, 'NNPDF31_nnlo_hessian_pdfas', 103],
              [2104, 322500, 'NNPDF31_nnlo_as_0108', 1],
              [2105, 322700, 'NNPDF31_nnlo_as_0110', 1],
              [2106, 322900, 'NNPDF31_nnlo_as_0112', 1],
              [2107, 323100, 'NNPDF31_nnlo_as_0114', 1],
              [2108, 323300, 'NNPDF31_nnlo_as_0117', 1],
              [2109, 323500, 'NNPDF31_nnlo_as_0119', 1],
              [2110, 323700, 'NNPDF31_nnlo_as_0122', 1],
              [2111, 323900, 'NNPDF31_nnlo_as_0124', 1],
              [3000, 305800, 'NNPDF31_nlo_hessian_pdfas', 103],
              [5000, 13000, 'CT14nnlo', 57],
              [5060, 13065, 'CT14nnlo_as_0116', 1],
              [5070, 13069, 'CT14nnlo_as_0120', 1],
              [4000, 13100, 'CT14nlo', 57],
              [4060, 13163, 'CT14nlo_as_0116', 1],
              [4070, 13167, 'CT14nlo_as_0120', 1],
              [4080, 13200, 'CT14lo', 1],
              [6000, 25200, 'MMHT2014nlo68clas118', 51],
              [7000, 25300, 'MMHT2014nnlo68cl', 51],
              [7060, 25000, 'MMHT2014lo68cl', 1],
              [8000, 42780, 'ABMP16als118_5_nnlo', 30],
              [8500, 90200, 'PDF4LHC15_nlo_100_pdfas', 103],
              [9000, 91200, 'PDF4LHC15_nnlo_100_pdfas', 103],
              [10000, 90400, 'PDF4LHC15_nlo_30_pdfas', 33],
              [11000, 91400, 'PDF4LHC15_nnlo_30_pdfas', 33],
              [12000, 61100, 'HERAPDF20_NLO_EIG', 29],
              [12050, 61130, 'HERAPDF20_NLO_VAR', 14],
              [13000, 61200, 'HERAPDF20_NNLO_EIG', 29],
              [13050, 61230, 'HERAPDF20_NNLO_VAR', 14],
              [14000, 13400, 'CT14qed_inc_proton', 31],
              [15000, 82200, 'LUXqed17_plus_PDF4LHC15_nnlo_100', 108],
            ],
            "PDF_variation2 , replica" :
            [
              [1500, 292200, 'NNPDF30_nlo_nf_5_pdfas', 103],
              [1700, 292600, 'NNPDF30_nnlo_nf_5_pdfas', 1],
              [1800, 315000, 'NNPDF31_lo_as_0118', 1],
              [1850, 315200, 'NNPDF31_lo_as_0130', 1],
              [1900, 262000, 'NNPDF30_lo_as_0118', 1],
              [1950, 263000, 'NNPDF30_lo_as_0130', 1],
            ],
          }
else:
  # 4F PDF    
  pdf_sets = {
            # weight id, LHAPDF id, name, replicas to be written
            "PDF_variation1 , hessian" :
            [
              [1500, 306000, 'NNPDF31_nnlo_hessian_pdfas', 103],
              [3400, 11082, 'CT10nlo_nf4', 2],
              [3450, 13091, 'CT14nnlo_NF4', 1],
              [3500, 13191, 'CT14nlo_NF4', 1],
              [3550, 13202, 'CT14lo_NF4', 1],
              [3600, 23100, 'MSTW2008lo68cl_nf4', 41],
              [3700, 23300, 'MSTW2008nlo68cl_nf4', 41],
              [3800, 23490, 'MSTW2008nlo_mbrange_nf4', 7],
              [3900, 23600, 'MSTW2008nnlo68cl_nf4', 41],
              [4000, 23790, 'MSTW2008nnlo_mbrange_nf4', 7],
              [4100, 25410, 'MMHT2014nlo68cl_nf4', 51],
              [4200, 25510, 'MMHT2014nlo68clas118_nf4', 51],
              [4300, 25570, 'MMHT2014nlo_asmzsmallrange_nf4', 5],
              [4400, 25605, 'MMHT2014nlo_mcrange_nf4', 9],
              [4500, 25620, 'MMHT2014nlo_mbrange_nf4', 5],
              [4600, 25710, 'MMHT2014nnlo68cl_nf4', 51],
              [4700, 25770, 'MMHT2014nnlo_asmzsmallrange_nf4', 3],
              [4800, 25805, 'MMHT2014nnlo_mcrange_nf4', 9],
              [4900, 25840, 'MMHT2014nnlo_mbrange_nf4', 5],
              [5000, 92000, 'PDF4LHC15_nlo_nf4_30', 31],
            ],
            "PDF_variation2 , replica" :
            [
              [2000, 320900, 'NNPDF31_nnlo_as_0118_nf_4', 101],
              [2200, 320500, 'NNPDF31_nlo_as_0118_nf_4', 101],
              [2400, 260400, 'NNPDF30_nlo_as_0118_nf_4', 101],
              [2600, 262400, 'NNPDF30_lo_as_0118_nf_4', 1],
              [2800, 263400, 'NNPDF30_lo_as_0130_nf_4', 1],
              [3000, 292000, 'NNPDF30_nlo_nf_4_pdfas', 103],
              [3200, 292400, 'NNPDF30_nnlo_nf_4_pdfas' ,1],
              
            ],
          }
  
for key, pdfsets in sorted(pdf_sets.iteritems()):
  weightgroup_name = key.replace(" ", "").split(',')[0]
  combine = key.replace(" ", "").split(',')[1]
  print 'weightgroup_name',weightgroup_name,'combine',combine
  fout.write("<weightgroup name='"+weightgroup_name+"' combine='"+combine+"' >\n")
  for pdf in pdfsets:
    print 'pdf',pdf
    m_idx = pdf[0]
    pdf_member_start = pdf[1]
    pdf_member_end = pdf[1] + pdf[3]
    for idx in range(pdf_member_start, pdf_member_end) :
      fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
      m_idx = m_idx + 1
  fout.write("</weightgroup>\n")

fout.write("</initrwgt>\n")

fout.close()
