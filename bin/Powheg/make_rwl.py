#!/usr/bin/env python

'''
Script for POWHEG weighting calculation
By Yuan CHAO  08/06/2016
'''

import sys
import os

if len(sys.argv) < 3:
    print("""
ERROR: Please specify if the Flavor scheme for which you want to define the weights is 5F (1) or 4F (0), and the central PDF
Example of usage for 5F:  python make_rwl.py 1 325300
Example of usage for 4F:  python make_rwl.py 0 325500
""")
    sys.exit(1)

is5FlavorScheme = str(sys.argv[1])
CentralPDF = str(sys.argv[2])
# ToDo: clean forMiNNLO,forX0jj up and define specific set of PDF which is called per process or via external argument (e.g. Run2UL)
# switch for time being to "Run2UL if you need UL style PDF set 
Period="Run3" # "Run2UL"
forMiNNLO = bool(int(sys.argv[3])) if len(sys.argv) > 3 else False
forX0jj = bool(int(sys.argv[4])) if len(sys.argv) > 4 else False
process = str(sys.argv[5]) if len(sys.argv) > 5 else ''


if forMiNNLO:
  CentralPDF=306000

# is5FlavorScheme = True
# CentralPDF = 325300
# processes4Flavor = ['ST_tch_4f', 'bbH', 'Wbb_dec', 'Wbbj',]

# if processName in processes4Flavor:
  # is5FlavorScheme = False
  # CentralPDF = 325500

# print 'INFO: The selected process '+processName+'uses',('5F' if is5FlavorScheme == True else '4F'),'PDF Flavor Scheme, using central PDF set', CentralPDF

m_outfile = 'pwg-rwl.dat'

m_factor = ['1d0', '2d0', '0.5d0']
m_idx = 1001

fout = open(m_outfile, 'w')

# scale variations
fout.write("<initrwgt>\n")
fout.write("<weightgroup name='scale_variation' combine='envelope' >\n")

for m_rensc in m_factor :
  for m_facsc in m_factor :
    fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(CentralPDF)+" renscfact="+ \
               m_rensc+" facscfact="+m_facsc+" </weight>\n")
    m_idx = m_idx + 1
		  
fout.write("</weightgroup>\n")

if forMiNNLO:
  # additional MiNNLO scale variations (NNPDF 3.0)
  fout.write("<weightgroup name='scale_variation_nnpdf30' combine='envelope' >\n")

  for m_rensc in m_factor :
    for m_facsc in m_factor :
      fout.write("<weight id='"+str(m_idx)+"'> lhapdf=303200 renscfact="+ \
                m_rensc+" facscfact="+m_facsc+" </weight>\n")
      m_idx = m_idx + 1
        
  fout.write("</weightgroup>\n")

  print("MiNNLO: PDF variations will be reduced for generation speed")
  # custom 5F PDF for MiNNLO
  pdf_sets = {
            # weight id, LHAPDF id, name, replicas to be written
            "PDF_variation1 , hessian" :
            [
              [2000, 306000, 'NNPDF31_nnlo_hessian_pdfas', 103],
              [2108, 323300, 'NNPDF31_nnlo_as_0117', 1],
              [2109, 323500, 'NNPDF31_nnlo_as_0119', 1],
              [2200, 331600, 'NNPDF40_nnlo_hessian_pdfas', 53],
              [2260, 332700, 'NNPDF40_nnlo_as_01160', 1],
              [2270, 333700, 'NNPDF40_nnlo_as_01120', 1],
              [2300, 332100, 'NNPDF40_nnlo_pch_as_01180', 1],
              [2400, 325900, 'NNPDF31_nnlo_as_0118_CMSW2_hessian_100', 101],
              [2600, 326100, 'NNPDF31_nnlo_as_0118_CMSW3_hessian_100', 101],
              [2800, 326300, 'NNPDF31_nnlo_as_0118_CMSW4_hessian_100', 101],
              [3000, 303200, 'NNPDF30_nnlo_as_0118_hessian', 101],
              [3103, 269000, 'NNPDF30_nnlo_as_0117', 1],
              [3104, 270000, 'NNPDF30_nnlo_as_0119', 1],
              [4000, 93300, 'PDF4LHC21_40_pdfas', 43],
              [5000, 14000, 'CT18NNLO', 59],
              [5070, 14066, 'CT18NNLO_as_0116', 1],
              [5071, 14067, 'CT18NNLO_as_0117', 1],
              [5072, 14069, 'CT18NNLO_as_0119', 1],
              [5073, 14070, 'CT18NNLO_as_0120', 1],
              [5100, 14100, 'CT18ZNNLO', 59],
              [5170, 14166, 'CT18ZNNLO_as_0116', 1],
              [5171, 14167, 'CT18ZNNLO_as_0117', 1],
              [5172, 14169, 'CT18ZNNLO_as_0119', 1],
              [5173, 14170, 'CT18ZNNLO_as_0120', 1],
              [6000, 27400, 'MSHT20nnlo_as118', 65],
              [6070, 27500, 'MSHT20nnlo_as_smallrange', 7],
              [6080, 27910, 'MSHT20nnlo_mcrange_nf5', 9],
              [6090, 27950, 'MSHT20nnlo_mbrange_nf5', 7],
              [7000, 25300, 'MMHT2014nnlo68cl', 51],
              [7060, 25360, 'MMHT2014nnlo_asmzsmallrange', 3],
              [8000, 42560, 'ABMP16_5_nnlo', 30],
              [8050, 43050, 'ABMP16als116_5_nlo', 1],
              [8051, 43110, 'ABMP16als118_5_nlo', 1],
              [8052, 43170, 'ABMP16als120_5_nlo', 1],
              [9000, 65200, 'ATLASepWZVjet20-EIG', 33],
              [10000, 65240, 'ATLASepWZVjet20-MOD', 9],
              [11000, 65250, 'ATLASepWZVjet20-PAR', 18],
              [13000, 61200, 'HERAPDF20_NNLO_EIG', 29],
              [13050, 61230, 'HERAPDF20_NNLO_VAR', 14],
              [13100, 61746, 'HERAPDF20_NNLO_ALPHAS_116', 1],
              [13200, 61750, 'HERAPDF20_NNLO_ALPHAS_120', 1],
              [29100, 29100, 'MSHT20an3lo_as118', 105],
              [29400, 29400, 'MSHT20an3lo_as_smallrange', 7],
            ],
          }
elif forX0jj:
  # 5F PDF
  print("X0jj: PDF variations will be reduced for generation speed")
  pdf_sets = {
            # weight id, LHAPDF id, name, replicas to be written
            "PDF_variation1 , hessian" :
            [ 
              [3200, 325300, 'NNPDF31_nnlo_as_0118_mc_hessian_pdfas', 103],
              [2000, 306000, 'NNPDF31_nnlo_hessian_pdfas', 1],
              [3000, 305800, 'NNPDF31_nlo_hessian_pdfas', 1],
            ],
  }
### sets for Run3 start up 
elif Period == "Run3":
    if int(is5FlavorScheme) == 1:
        print("Going to use Run 3 5FS PDFs")
        # 5F PDF
        pdf_sets = {
            # weight id, LHAPDF id, name, replicas to be written
            "PDF_variation1 , hessian" :
            [
                [2000, 325300, 'NNPDF31_nnlo_as_0118_mc_hessian_pdfas', 103],
                [2200, 306000, 'NNPDF31_nnlo_hessian_pdfas', 1],
                [2201, 322500, 'NNPDF31_nnlo_as_0108', 1],
                [2202, 322700, 'NNPDF31_nnlo_as_0110', 1],
                [2203, 322900, 'NNPDF31_nnlo_as_0112', 1],
                [2204, 323100, 'NNPDF31_nnlo_as_0114', 1],
                [2205, 323300, 'NNPDF31_nnlo_as_0117', 1],
                [2206, 323500, 'NNPDF31_nnlo_as_0119', 1],
                [2207, 323700, 'NNPDF31_nnlo_as_0122', 1],
                [2208, 323900, 'NNPDF31_nnlo_as_0124', 1],
                [2300, 305800, 'NNPDF31_nlo_hessian_pdfas', 103],
                [2500, 303200, 'NNPDF30_nnlo_as_0118_hessian', 1],
                [2501, 292200, 'NNPDF30_nlo_nf_5_pdfas', 1],
                [2600, 331600, 'NNPDF40_nnlo_hessian_pdfas', 53],
                [2700, 332700, 'NNPDF40_nnlo_as_01160', 1],
                [2701, 332900, 'NNPDF40_nnlo_as_01170', 1],
                [2702, 333100, 'NNPDF40_nnlo_as_01175', 1],
                [2703, 333300, 'NNPDF40_nnlo_as_01185', 1],
                [2704, 333500, 'NNPDF40_nnlo_as_01190', 1],
                [2705, 333700, 'NNPDF40_nnlo_as_01200', 1],
                [2800, 332300, 'NNPDF40_nlo_pch_as_01180', 1], 
                [4000, 14000, 'CT18NNLO', 59],
                [4100, 14066, 'CT18NNLO_as_0116', 1], 
                [4101, 14067, 'CT18NNLO_as_0117', 1], 
                [4102, 14069, 'CT18NNLO_as_0119', 1], 
                [4103, 14070, 'CT18NNLO_as_0120', 1], 
                [4200, 14100, 'CT18ZNNLO', 59],
                [4300, 14200, 'CT18ANNLO', 1],
                [4301, 14300, 'CT18XNNLO', 1],
                [5000, 27400, 'MSHT20nnlo_as118', 65],
                [5100, 27500, 'MSHT20nnlo_as_smallrange', 1],
                [5101, 27550, 'MSHT20nnlo_as_largerange', 1],
                [6000, 93300, 'PDF4LHC21_40_pdfas', 43],
                [7000, 61200, 'HERAPDF20_NNLO_EIG', 29],
                [8000, 42780, 'ABMP16als118_5_nnlo', 30],
            ],
            "PDF_variation2 , replica" :
            [
                [3000, 316200, 'NNPDF31_nnlo_as_0118_mc', 101],
                [3200, 331300, 'NNPDF40_nnlo_pdfas', 103],
                [3400, 332100, 'NNPDF40_nnlo_pch_as_01180', 101],
            ],
        }
    else: 
        print("Going to use Run 3 4FS PDFs")
        # 4F PDF    
        pdf_sets = {
            # weight id, LHAPDF id, name, replicas to be written
            "PDF_variation1 , hessian" :
            [
                [2000, 325500, 'NNPDF31_nnlo_as_0118_nf_4_mc_hessian', 101],
                [2200, 320500, 'NNPDF31_nlo_as_0118_nf_4', 1],
                [2500, 292000, 'NNPDF30_nlo_nf_4_pdfas', 1],
                [2600, 335700, 'NNPDF40_nlo_nf_4_pdfas', 1],
                [4000, 13091, 'CT14nnlo_NF4', 1],
                [4001, 13191, 'CT14nlo_NF4', 1],
                [5000, 27810, 'MSHT20nnlo_nf4', 65],
                [5100, 27870, 'MSHT20nnlo_as_smallrange_nf4', 3],
                [5200, 27610, 'MSHT20nlo_nf4', 1],
                [6000, 93700, 'PDF4LHC21_40_pdfas_nf4', 43],
                [7000, 42530, 'ABMP16_4_nnlo', 30],
                [7100, 42930, 'ABMP16_4_nlo', 1],
            ],
            "PDF_variation2 , replica" :
            [
                [3000, 320900, 'NNPDF31_nnlo_as_0118_nf_4', 101],
                [3200, 292400, 'NNPDF30_nnlo_nf_4_pdfas', 103],                
                [3400, 335500, 'NNPDF40_nnlo_nf_4_pdfas', 103],                
            ],
        }
### sets for Run2 Ultra Legacy         
elif Period == "Run2UL":
    if int(is5FlavorScheme) == 1:
        print("Going to use Run 2 UL 5FS PDFs")
        # 5F PDF
        pdf_sets = {
            # weight id, LHAPDF id, name, replicas to be written
            "PDF_variation1 , hessian" :
            [
                [3200, 325300, 'NNPDF31_nnlo_as_0118_mc_hessian_pdfas', 103],
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
            ],
            "PDF_variation2 , replica" :
            [
                [3400, 316200, 'NNPDF31_nnlo_as_0118_mc', 101],
                [1500, 292200, 'NNPDF30_nlo_nf_5_pdfas', 103],
                [1700, 292600, 'NNPDF30_nnlo_nf_5_pdfas', 1],
                [1800, 315000, 'NNPDF31_lo_as_0118', 1],
                [1850, 315200, 'NNPDF31_lo_as_0130', 1],
                [1900, 262000, 'NNPDF30_lo_as_0118', 1],
                [1950, 263000, 'NNPDF30_lo_as_0130', 1],
                [15000, 82200, 'LUXqed17_plus_PDF4LHC15_nnlo_100', 108],
                [16000, 325100, 'NNPDF31_nnlo_as_0118_luxqed', 101],
            ],
        }
    else:
        print("Going to use Run 2 UL 4FS PDFs")
        # 4F PDF    
        pdf_sets = {
            # weight id, LHAPDF id, name, replicas to be written
            "PDF_variation1 , hessian" :
            [
                [1500, 325500, 'NNPDF31_nnlo_as_0118_nf_4_mc_hessian', 101],
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
# no idea what to do 
else:
    print("Unclear which PDF to use. Please check make_rwl.py")

### start generting the pdf file 
pdf_count = 0
for key, pdfsets in sorted(pdf_sets.items()):
  weightgroup_name = key.replace(" ", "").split(',')[0]
  combine = key.replace(" ", "").split(',')[1]
  print('weightgroup_name',weightgroup_name,'combine',combine)
  fout.write("<weightgroup name='"+weightgroup_name+"' combine='"+combine+"' >\n")
  for pdf in pdfsets:
    print('pdf',pdf)
    m_idx = pdf[0]
    pdf_member_start = pdf[1]
    pdf_member_end = pdf[1] + pdf[3]
    for idx in range(pdf_member_start, pdf_member_end) :
      fout.write("<weight id='"+str(m_idx)+"'> lhapdf="+str(idx)+" </weight>\n")
      m_idx = m_idx + 1
      pdf_count += 1
  fout.write("</weightgroup>\n")

fout.write("</initrwgt>\n")

fout.close()

print('pdf_count = ',pdf_count)
