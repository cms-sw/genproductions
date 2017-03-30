#! /bin/env python

import os, shutil, subprocess

Reference_dir = "WpToTpBottom_conf"

FinalStates = ["Wb"]
#FinalStates = ["Wb","Ht","Zt"]
Chiralities =["LH","RH"]
Masses = [[1500,[700,900,1200]],[2000,[900,1200,1500]],[2500,[1200,1500,1900]]]
Wwidths = [[0.3,'Nar'],[0.5,'Wid']]
Twidths = [[0.3,'Nar'],[0.5,'Wid']]

for state in FinalStates:
	for hand in Chiralities:
		for wmass in Masses:
			for tmass in wmass[1]:
				for wwidth in Wwidths:
					for twidth in Twidths:
						if  hand=='RH' and not (
							(wmass[0]==1500 and tmass==700 and wwidth[1]=='Nar' and twidth[1]=='Nar') or 
							(wmass[0]==2000 and tmass==1200 and wwidth[1]=='Nar' and twidth[1]=='Nar') or 
							(wmass[0]==2500 and tmass==1900 and wwidth[1]=='Nar' and twidth[1]=='Nar') ):
							continue
						if twidth[1]=='Wid' and wwidth[1]=='Nar' and not (
							(wmass[0]==1500 and tmass==700 and hand=='LH') or
							(wmass[0]==2000 and tmass==1200 and hand=='LH') or
							(wmass[0]==2500 and tmass==1900 and hand=='LH') ):
							continue
						if wwidth[1]=='Wid' and twidth[1]=='Nar' and not (
							(wmass[0]==1500 and tmass==700 and hand=='LH') or
							(wmass[0]==2000 and tmass==1200 and hand=='LH') or
							(wmass[0]==2500 and tmass==1900 and hand=='LH') ):
							continue
                                               #if wwidth[1]=='Wid' and twidth[1]=='Wid' and not (
							#(wmass[0]==1500 and tmass==700 and hand=='LH') or
                                                        #(wmass[0]==2000 and tmass==1200 and hand=='LH') ):
							#(wmass[0]==2500 and tmass==1900 and hand=='LH') ):
                                                        #continue
						#create name
						sampleName = 'WpTpb_Wp'+str(wmass[0])+wwidth[1]+'_Tp'+str(tmass)+twidth[1]+hand+'_'+state
						print sampleName
						#remove dir if already exists and create
						if os.path.isdir(sampleName):
							shutil.rmtree(sampleName)
						os.makedirs(sampleName)
						# Copy cards
						shutil.copyfile(Reference_dir+'/'+'run_card.dat',sampleName+'/'+sampleName+'_run_card.dat')
                                                shutil.copyfile(Reference_dir+'/'+state+'_madspin_card.dat',sampleName+'/'+sampleName+'_madspin_card.dat')
                                                shutil.copyfile(Reference_dir+'/'+'extramodels.dat',sampleName+'/'+sampleName+'_extramodels.dat')
						shutil.copyfile(Reference_dir+'/'+state+'_proc_card.dat',sampleName+'/'+sampleName+'_proc_card.dat')
						shutil.copyfile(Reference_dir+'/'+state+'_'+hand+'_customizecards.dat',sampleName+'/'+sampleName+'_customizecards.dat')
						#customization
						with open("{0}/{0}_proc_card.dat".format(sampleName), "a") as f:
							f.write("output "+sampleName)
						with open("{0}/{0}_customizecards.dat".format(sampleName), "a") as f:
							f.write("set param_card mass 8000001 %e\n" % tmass)
							f.write("set param_card mass 9900213 %e\n" % wmass[0])
							f.write("set param_card decay 9900213 %e\n" % (wmass[0] * wwidth[0]))
							if twidth[0]==0:
								f.write("set param_card decay 8000001 %e" % 0.001)
							else:
								f.write("set param_card decay 8000001 %e" % (tmass * twidth[0]))
