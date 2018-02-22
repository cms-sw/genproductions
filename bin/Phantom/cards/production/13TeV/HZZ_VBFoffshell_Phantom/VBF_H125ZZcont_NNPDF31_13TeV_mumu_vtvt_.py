[general]
CMSSW = CMSSW_9_3_0
package = https://cms-project-generators.web.cern.ch/cms-project-generators/phantom/compiled/phantom_1_3_p1_slc6_amd64_gcc630.tar.gz
ARCH = slc6_amd64_gcc630
foldername    =   gridpack_VBF_H125ZZcont_NNPDF31_13TeV_mumu_vtvt_

[submission]
scheduler = LSF
queue = 1nw

[generation]
channel    =   mu mu_ vt vt_
topnumber = -1
excludegluons = 1

[parameters]
PDFname = NNPDF31_nnlo_hessian_pdfas
perturbativeorder = 1
rmh = 125
gamh = 0.004088
ghfactor = 1
i_e_min_lep = 1
e_min_lep = 3.d0
i_pt_min_lep = 1
pt_min_lep = 3.d0
i_eta_max_lep = 1
eta_max_lep = 2.7d0
i_eta_max_j = 1
eta_max_j = 6.5d0
i_rm_min_jj = 1
rm_min_jj = 30.d0
i_rm_min_ll = 1
rm_min_ll = 4.d0
i_rm_min_4l = 1
rm_min_4l = 70.d0
i_rm_min_jfjb = 0
i_PDFscale = 4
fixed_PDFscale = 125
ecoll = 13000
i_signal = 3
i_singlet = 0
rmhh = 600.d0
rcosa = 0.9
tgbeta = 2.d0
gamhh = -12
i_hh = 0
rmhh_ns = 500.d0
ghhfactor = -1.d0
gamhh_ns = -41.62d0

