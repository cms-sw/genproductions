import sys
from histograms import *
import ROOT

################################################################################
#  CHOICE OF INPUT FILE
################################################################################
# check if an argument is passed as inputfiles:
if len(sys.argv) >1:
    input_file = sys.argv[1]
else:
    #take the default path
    print('ERROR! please provide djrs.dat')
print("Reading information from: ", input_file)


################################################################################
#  PARSING THE FILE AND ACCESS TO BASIC PROPERTY OF THE OBJECT
################################################################################
#parsing the data and create the object instance 
hwu_list = HwUList(input_file, raw_labels=True)
# raw label prevent modification of the weight label. They will stay at their inputfile value

# get the list of the plot names
names =  hwu_list.get_hist_names()
print(names)
# get the list of the weight label
weights_name = hwu_list.get_wgt_names()
print(weights_name)
hist_01 = [hwu_list.get(n) for n in names if 'd01' in n]
hist_12 = [hwu_list.get(n) for n in names if 'd12' in n]
hist_23 = [hwu_list.get(n) for n in names if 'd23' in n]
hist_34 = [hwu_list.get(n) for n in names if 'd34' in n]
hist_45 = [hwu_list.get(n) for n in names if 'd45' in n]

h_01_0j_temp = [h for h in hist_01 if 'Jet sample 0' in h.get_HwU_histogram_name()]
h_01_1j_temp = [h for h in hist_01 if 'Jet sample 1' in h.get_HwU_histogram_name()]
h_01_2j_temp = [h for h in hist_01 if 'Jet sample 2' in h.get_HwU_histogram_name()]
h_01_3j_temp = [h for h in hist_01 if 'Jet sample 3' in h.get_HwU_histogram_name()]
h_01_4j_temp = [h for h in hist_01 if 'Jet sample 4' in h.get_HwU_histogram_name()]
h_12_0j_temp = [h for h in hist_12 if 'Jet sample 0' in h.get_HwU_histogram_name()]
h_12_1j_temp = [h for h in hist_12 if 'Jet sample 1' in h.get_HwU_histogram_name()]
h_12_2j_temp = [h for h in hist_12 if 'Jet sample 2' in h.get_HwU_histogram_name()]
h_12_3j_temp = [h for h in hist_12 if 'Jet sample 3' in h.get_HwU_histogram_name()]
h_12_4j_temp = [h for h in hist_12 if 'Jet sample 4' in h.get_HwU_histogram_name()]
h_23_0j_temp = [h for h in hist_23 if 'Jet sample 0' in h.get_HwU_histogram_name()]
h_23_1j_temp = [h for h in hist_23 if 'Jet sample 1' in h.get_HwU_histogram_name()]
h_23_2j_temp = [h for h in hist_23 if 'Jet sample 2' in h.get_HwU_histogram_name()]
h_23_3j_temp = [h for h in hist_23 if 'Jet sample 3' in h.get_HwU_histogram_name()]
h_23_4j_temp = [h for h in hist_23 if 'Jet sample 4' in h.get_HwU_histogram_name()]
h_34_0j_temp = [h for h in hist_34 if 'Jet sample 0' in h.get_HwU_histogram_name()]
h_34_1j_temp = [h for h in hist_34 if 'Jet sample 1' in h.get_HwU_histogram_name()]
h_34_2j_temp = [h for h in hist_34 if 'Jet sample 2' in h.get_HwU_histogram_name()]
h_34_3j_temp = [h for h in hist_34 if 'Jet sample 3' in h.get_HwU_histogram_name()]
h_34_4j_temp = [h for h in hist_34 if 'Jet sample 4' in h.get_HwU_histogram_name()]

if len(h_01_0j_temp)>0:h_01_0j=h_01_0j_temp[0]
if len(h_01_1j_temp)>0:h_01_1j=h_01_1j_temp[0]
if len(h_01_2j_temp)>0:h_01_2j=h_01_2j_temp[0]
if len(h_01_3j_temp)>0:h_01_3j=h_01_3j_temp[0]
if len(h_01_4j_temp)>0:h_01_4j=h_01_4j_temp[0]
if len(h_12_0j_temp)>0:h_12_0j=h_12_0j_temp[0]
if len(h_12_1j_temp)>0:h_12_1j=h_12_1j_temp[0]
if len(h_12_2j_temp)>0:h_12_2j=h_12_2j_temp[0]
if len(h_12_3j_temp)>0:h_12_3j=h_12_3j_temp[0]
if len(h_12_4j_temp)>0:h_12_4j=h_12_4j_temp[0]
if len(h_23_0j_temp)>0:h_23_0j=h_23_0j_temp[0]
if len(h_23_1j_temp)>0:h_23_1j=h_23_1j_temp[0]
if len(h_23_2j_temp)>0:h_23_2j=h_23_2j_temp[0]
if len(h_23_3j_temp)>0:h_23_3j=h_23_3j_temp[0]
if len(h_23_4j_temp)>0:h_23_4j=h_23_4j_temp[0]
if len(h_34_0j_temp)>0:h_34_0j=h_34_0j_temp[0]
if len(h_34_1j_temp)>0:h_34_1j=h_34_1j_temp[0]
if len(h_34_2j_temp)>0:h_34_2j=h_34_2j_temp[0]
if len(h_34_3j_temp)>0:h_34_3j=h_34_3j_temp[0]
if len(h_34_4j_temp)>0:h_34_4j=h_34_4j_temp[0]

h_01_array=[]
h_12_array=[]
h_23_array=[]
h_34_array=[]

if 'h_01_0j' in vars():h_01_array.append(h_01_0j)
if 'h_01_1j' in vars():h_01_array.append(h_01_1j)
if 'h_01_2j' in vars():h_01_array.append(h_01_2j)
if 'h_01_3j' in vars():h_01_array.append(h_01_3j)
if 'h_01_4j' in vars():h_01_array.append(h_01_4j)
if 'h_12_0j' in vars():h_12_array.append(h_12_0j)
if 'h_12_1j' in vars():h_12_array.append(h_12_1j)
if 'h_12_2j' in vars():h_12_array.append(h_12_2j)
if 'h_12_3j' in vars():h_12_array.append(h_12_3j)
if 'h_12_4j' in vars():h_12_array.append(h_12_4j)
if 'h_23_0j' in vars():h_23_array.append(h_23_0j)
if 'h_23_1j' in vars():h_23_array.append(h_23_1j)
if 'h_23_2j' in vars():h_23_array.append(h_23_2j)
if 'h_23_3j' in vars():h_23_array.append(h_23_3j)
if 'h_23_4j' in vars():h_23_array.append(h_23_4j)
if 'h_34_0j' in vars():h_34_array.append(h_34_0j)
if 'h_34_1j' in vars():h_34_array.append(h_34_1j)
if 'h_34_2j' in vars():h_34_array.append(h_34_2j)
if 'h_34_3j' in vars():h_34_array.append(h_34_3j)
if 'h_34_4j' in vars():h_34_array.append(h_34_4j)

hists_01=[]
hists_12=[]
hists_23=[]
hists_34=[]

hist_bins_array=h_01_array[0].get("bins")
bins=len(hist_bins_array)-1
hist_temp=ROOT.TH1D('','',bins,hist_bins_array[0],hist_bins_array[-1])

for i in range(0,len(h_01_array)):
  for ib in range(0,bins):
    hist_temp.SetBinContent(ib+1,h_01_array[i].get(weights_name[0])[ib])
    hist_temp.SetBinError(ib+1,h_01_array[i].get(weights_name[1])[ib])
  hists_01.append(hist_temp.Clone())

#Rebin
#for i in range(0,len(h_01_array)):
#  hists_01[i].Rebin(2)

for i in range(0,len(h_12_array)):
  for ib in range(0,bins):
    hist_temp.SetBinContent(ib+1,h_12_array[i].get(weights_name[0])[ib])
    hist_temp.SetBinError(ib+1,h_12_array[i].get(weights_name[1])[ib])
  hists_12.append(hist_temp.Clone())

#Rebin
#for i in range(0,len(h_12_array)):
#  hists_12[i].Rebin(2)

for i in range(0,len(h_23_array)):
  for ib in range(0,bins):
    hist_temp.SetBinContent(ib+1,h_23_array[i].get(weights_name[0])[ib])
    hist_temp.SetBinError(ib+1,h_23_array[i].get(weights_name[1])[ib])
  hists_23.append(hist_temp.Clone())

#Rebin
#for i in range(0,len(h_23_array)):
#  hists_23[i].Rebin(2)

for i in range(0,len(h_34_array)):
  for ib in range(0,bins):
    hist_temp.SetBinContent(ib+1,h_34_array[i].get(weights_name[0])[ib])
    hist_temp.SetBinError(ib+1,h_34_array[i].get(weights_name[1])[ib])
  hists_34.append(hist_temp.Clone())

#Rebin
#for i in range(0,len(h_34_array)):
#  hists_34[i].Rebin(2)

colors=[600,629,419,810,30]

def plots(histos, idjr):
  c1=ROOT.TCanvas('','',600,600)
  c1.cd()
  hist_all=histos[0].Clone()
  hist_all.SetStats(0)
  if idjr==0:hist_all.GetXaxis().SetTitle("DJR 0->1")
  if idjr==1:hist_all.GetXaxis().SetTitle("DJR 1->2")
  if idjr==2:hist_all.GetXaxis().SetTitle("DJR 2->3")
  if idjr==3:hist_all.GetXaxis().SetTitle("DJR 3->4")

  hist_all.Reset()
  for i in range(0,len(histos)):
    hist_all.Add(histos[i])
  hist_all.SetLineWidth(1)
  hist_all.Draw('he')

  for i in range(0,len(histos)):
    histos[i].SetLineStyle(2)
    histos[i].SetLineColor(colors[i])
    histos[i].Draw('he same')
    
  leg1=ROOT.TLegend(0.67,0.87-4*0.06,0.87,0.87)
  leg1.SetTextSize(0.035)
  leg1.SetBorderSize(0)
  leg1.SetTextFont(62)
  leg1.SetLineColor(0)
  leg1.SetLineStyle(1)
  leg1.SetLineWidth(1)
  leg1.SetFillColor(0)
  leg1.SetFillStyle(1001)
  leg1.AddEntry(hist_all,'all partons')
  for i in range(0,len(histos)):
    leg1.AddEntry(histos[i],str(i)+' parton')
  leg1.Draw()
  
  if idjr==0:
    c1.SaveAs('DJR01.pdf')
    c1.SaveAs('DJR01.png')
  if idjr==1:
    c1.SaveAs('DJR12.pdf')
    c1.SaveAs('DJR12.png')
  if idjr==2:
    c1.SaveAs('DJR23.pdf')
    c1.SaveAs('DJR23.png')
  if idjr==3:
    c1.SaveAs('DJR34.pdf')
    c1.SaveAs('DJR34.png')

plots(hists_01,0)
plots(hists_12,1)
plots(hists_23,2)
plots(hists_34,3)
