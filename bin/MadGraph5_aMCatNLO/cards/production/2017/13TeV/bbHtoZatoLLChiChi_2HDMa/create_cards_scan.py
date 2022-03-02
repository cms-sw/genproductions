#!/usr/bin/env python3

import os,sys
import time, json

##############################################
def rwgt_card(mH,ma,width_dict):
##############################################
	dict_rwgt_default = {'frblock 2':10, 'higgs 1':0.5,'higgs 2':0.5,'higgs 3':0.5, 'higgs 5':0.5, 'dminputs 1':1}
	dict_block_names = {'tanbeta':'frblock 2', 'lam3':'higgs 1', 'laP1':'higgs 2', 'laP2':'higgs 3', 'sinp':'higgs 5'}
	dict_rwgt_scan = {}
	dict_rwgt_scan['tanbeta'] = [5,10,15,20,30]
	dict_rwgt_scan['sinp'] = [0.1,0.3,0.5,0.7,0.9]
	dict_rwgt_scan['lam3'] = [0.3,0.4,0.5,0.6,0.7]
	
	rew_card = 'change rwgt_dir rwgt\n'
	for v1 in dict_rwgt_scan['tanbeta']:
		for v2 in dict_rwgt_scan['sinp']:
			for v3 in dict_rwgt_scan['lam3']:
				rew_card += f'launch --rwgt_name=parvar_tb{str(v1)}_sp{str(v2).replace(".","p")}_l3{str(v3).replace(".","p")}\n'
				rew_card += f'    set {dict_block_names["tanbeta"]} {v1}\n'
				rew_card += f'    set {dict_block_names["sinp"]} {v2}\n'
				rew_card += f'    set {dict_block_names["lam3"]} {v3}\n'
				rew_card += f'    set {dict_block_names["laP1"]} {v3}\n'
				rew_card += f'    set {dict_block_names["laP2"]} {v3}\n'
				rew_card += f'    set mass 35 {mH}\n'
				rew_card += f'    set mass 36 {mH}\n'
				rew_card += f'    set mass 37 {mH}\n'
				rew_card += f'    set mass 54 {ma}\n'
				rew_card += f'    set decay 35 {width_dict[str(mH)][str(ma)][str(v1)][str(v2)][str(v3)]["H"]}\n'
				rew_card += f'    set decay 54 {width_dict[str(mH)][str(ma)][str(v1)][str(v2)][str(v3)]["a"]}\n'
				
	return rew_card

##############################################
def main():
##############################################
	run_dir = "/nfs/dust/cms/user/perezdan/MC_Production/2016/bbH_Za_LLChiChi/genproductions/bin/MadGraph5_aMCatNLO/cards/production/13TeV/bbH_Za_LLChiChi"
	out_dir = "/nfs/dust/cms/user/perezdan/MC_Production/2016/bbH_Za_LLChiChi/genproductions/bin/MadGraph5_aMCatNLO/cards/production/13TeV/bbH_Za_LLChiChi/parameter_scan"
	
	
	if(not os.path.exists(out_dir)):
		os.system(f"mkdir {out_dir}")
	else:
		os.system(f"rm -rf {out_dir}/*")
	
	
	with open('width_scan.json', 'r') as json_file:
		width_dict = json.load(json_file)
	
	for MH in ['400','500','600','800','1000']:
		
		for Ma in ['100','200','300','400','600','800']:
			
			if int(Ma)+2*90>int(MH):
				continue
			
			############# copying and modifying proc_card ###############
			#open template card
			fin = open(run_dir+"/bbH_Za_LLChiChi_4f_proc_card.dat", "rt")
			proc_card = fin.read()
			proc_card = proc_card.replace('bbH_Za_LLChiChi_4f', f'bbH_Za_LLChiChi_MH{MH}_Ma{Ma}_MChi45_4f')
			fin.close()
			#open the point specific card 
			fin = open(out_dir+f'/bbH_Za_LLChiChi_MH{MH}_Ma{Ma}_MChi45_4f_proc_card.dat', "wt")
			fin.write(proc_card)
			fin.close()
			
			############# copying and modifying customizecards ###############
			#open template card
			fin = open(run_dir+"/bbH_Za_LLChiChi_4f_customizecards.dat", "rt")
			cust_card = fin.read()
			cust_card = cust_card.replace('herereplacemassofH', f'{MH}')
			cust_card = cust_card.replace('herereplacemassofa', f'{Ma}')
			cust_card = cust_card.replace('herereplacewidthofa', f'{width_dict[MH][Ma]["10"]["0.5"]["0.5"]["a"]}')
			cust_card = cust_card.replace('herereplacewidthofH', f'{width_dict[MH][Ma]["10"]["0.5"]["0.5"]["H"]}')
			fin.close()
			#open the point specific card 
			fin = open(out_dir+f'/bbH_Za_LLChiChi_MH{MH}_Ma{Ma}_MChi45_4f_customizecards.dat', "wt")
			fin.write(cust_card)
			fin.close()
			
			############# copying extramodels ###############
			fin = open(run_dir+"/bbH_Za_LLChiChi_4f_extramodels.dat", "rt")
			extra_card = fin.read()
			fin.close()
			fin = open(out_dir+f'/bbH_Za_LLChiChi_MH{MH}_Ma{Ma}_MChi45_4f_extramodels.dat', "wt")
			fin.write(extra_card)
			fin.close()
			
			############# copying run_card ###############
			fin = open(run_dir+"/bbH_Za_LLChiChi_4f_run_card.dat", "rt")
			run_card = fin.read()
			fin.close()
			fin = open(out_dir+f'/bbH_Za_LLChiChi_MH{MH}_Ma{Ma}_MChi45_4f_run_card.dat', "wt")
			fin.write(run_card)
			fin.close()
			
			############# creating reweight_card ###############
			rew_card = rwgt_card(int(MH),int(Ma),width_dict)
			fin = open(out_dir+f'/bbH_Za_LLChiChi_MH{MH}_Ma{Ma}_MChi45_4f_reweight_card.dat', "wt")
			fin.write(rew_card)
			fin.close()


if __name__ == "__main__":
	main()
