#! /bin/env python
import os
from shutil import copyfile

class Cards():
    def __init__(self,Mass_gKK_R):
        self.Mass_gKK_R = Mass_gKK_R
        self.TempleteFolder = "Res1ToRes2GluToGluVV_M1_R"
        self.Res1ToRes2GluToGluVV_M1_R_customizecards = "Res1ToRes2GluToGluVV_M1_R_customizecards.dat"
        self.Res1ToRes2GluToGluVV_M1_R_extramodels = "Res1ToRes2GluToGluVV_M1_R_extramodels.dat"
        self.Res1ToRes2GluToGluVV_M1_R_proc_card = "Res1ToRes2GluToGluVV_M1_R_proc_card.dat"
        self.Res1ToRes2GluToGluVV_M1_R_run_card = "Res1ToRes2GluToGluVV_M1_R_run_card.dat"

    def Cards(self):
        for mgKK,mR in self.Mass_gKK_R:
            BASEDIR = "Res1ToRes2GluToGluVV_M1-%s_R-%s"%(str(mgKK),str(mR))
            if not os.path.isfile(BASEDIR):
                os.makedirs(BASEDIR)
            with open("%s/%s"%(self.TempleteFolder,self.Res1ToRes2GluToGluVV_M1_R_customizecards),"r") as fin, open("%s/%s_customizecards.dat"%(BASEDIR,BASEDIR),"w") as fout:
                fout.write(fin.read().format(gKK_mass = str(mgKK),Radion_mass =(mR)))
            with open("%s/%s"%(self.TempleteFolder,self.Res1ToRes2GluToGluVV_M1_R_proc_card),"r") as fin, open("%s/%s_proc_card.dat"%(BASEDIR,BASEDIR),"w") as fout:
                fout.write(fin.read().format(gKK_mass = str(mgKK),Radion_mass =(mR)))
            copyfile("%s/%s"%(self.TempleteFolder,self.Res1ToRes2GluToGluVV_M1_R_run_card),"%s/%s_run_card.dat"%(BASEDIR,BASEDIR))
            copyfile("%s/%s"%(self.TempleteFolder,self.Res1ToRes2GluToGluVV_M1_R_extramodels),"%s/%s_extramodels.dat"%(BASEDIR,BASEDIR))

Mass_gKK_R = [
    (1500,180),(1500,250),(1500,330),(1500,500),(1500,620),(1500,750),(1500,1000),(1500,1200),(1500,1350),
    (2000,180),(2000,250),(2000,350),(2000,500),(2000,650),(2000,1000),(2000,1300),(2000,1600),(2000,1800),
    (2500,180),(2500,250),(2500,350),(2500,500),(2500,850),(2500,1250),(2500,1650),(2500,2000),(2500,2250),
    (3000,180),(3000,250),(3000,360),(3000,540),(3000,1000),(3000,1500),(3000,2000),(3000,2400),(3000,2700),
    (3500,180),(3500,250),(3500,420),(3500,630),(3500,1200),(3500,1750),(3500,2300),(3500,2800),(3500,3150),
    (4000,180),(4000,250),(4000,440),(4000,720),(4000,1300),(4000,2000),(4000,2600),(4000,3200),(4000,3600),
    (4500,180),(4500,250),(4500,450),(4500,810),(4500,1440),(4500,2250),(4500,3000),(4500,3600),(4500,4050),
]
cards = Cards(Mass_gKK_R)
cards.Cards()