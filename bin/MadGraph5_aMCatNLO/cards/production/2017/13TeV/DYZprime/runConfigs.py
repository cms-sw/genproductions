#!/bin/env python

import shutil
from string import Template
import random
import subprocess
import glob
# import wprimeWidth
#import zprimeWidth
from math import sqrt,pi
import numpy as np

kv=[0.0]
gl=[0.1, 0.5, 1., 6., 16.]
#gl=[0.,1.]
gh=gl
zprimeMasses=[250., 500., 750., 1000., 1250., 1500., 1750., 2000., 2250., 2500., 2750., 3000., 3500., 4000., 5000.]
zprimeWidths=[
    [0.742313,3.71156,7.42313,44.5388,118.77],
    [1.48463,7.42313,14.8463,89.0775,237.54],
    [2.22694,11.1347,22.2694,133.616,356.31],
    [2.96925,14.8463,29.6925,178.155,475.08],
    [3.71156,18.5578,37.1156,222.694,593.85],
    [4.45388,22.2694,44.5388,267.233,712.62],
    [5.19619,25.9809,51.9619,311.771,831.39],
    [5.9385,29.6925,59.385,356.31,950.16],
    [6.68081,33.4041,66.8081,400.849,1068.93],
    [7.42313,37.1156,74.2313,445.388,1187.7],
    [8.16544,40.8272,81.6544,489.926,1306.47],
    [8.90775,44.5388,89.0775,534.465,1425.24],
    [10.3924,51.9619,103.924,623.543,1662.78],
    [11.877,59.385,118.77,712.62,1900.32],
    [14.8463,74.2313,148.463,890.755,2375.4]
]

#mw=80.2673592
#mw=80

sinthetaW=0.23126
cos2thetaW=1.-sinthetaW


mZ=91.1876
mw=(cos2thetaW* mZ**2 )**0.5


for i_mzp in zprimeMasses:
    for i_gl in gl:
        for i_gh in [1.0]:
            i_gh=i_gl
            for i_kv in kv:

                name=("Zprime_NonUniversalSSM_M%s_gl%g_gh%g_kv%s"%(i_mzp,i_gl,i_gh,0.0)).replace(".","p")
                print("generating %s"%name)


                gzpvv=i_kv*(5.3*mw/i_mzp)**2.
                ch=1.
                width=zprimeWidths[zprimeMasses.index(i_mzp)][gl.index(i_gl)]
                #width=zprimeWidth.zprimeTotal_pdg(i_mzp,i_gl,i_gh,gzpvv,ch)
                d = dict(
                    MZPRIME='%se+00'%(i_mzp),
                    WZPRIME='%g'%(width),
                    GL='%g'%(i_gl),
                    GH='%g'%(i_gh),
                    GZPVV='%g'%(gzpvv),
                )
                #print( "mzp: {0}    mwp_width: {1}  gl: {2}    gh: {3} mw: {4}\n\n".format(i_mzp,width, i_gl, i_gh, gzpvv)  )

                #file=open("param_card.template","r")
                file=open("customizecards.template","r")
                text=file.read()
                file.close()
                newText=Template(text).safe_substitute(d)
                #fileNew=open("%s_param_card.dat"%(name),"w+")
                fileNew=open("%s_customizecards.dat"%(name),"w+")
                fileNew.write(newText)
                fileNew.close()
                
                model="ssmzp_nonuniversal_v4"
                if i_gl==0:
                        model="ssmzp_nonuniversal_no_light_v4"
                d = dict(
                    modelfile="%s"%(model),
                    NAME="%s"%(name),
                )
                file=open("proc_card_mg5.template","r")
                text=file.read()
                file.close()
                newText=Template(text).safe_substitute(d)
                fileNew=open("%s_proc_card.dat"%(name),"w+")
                fileNew.write(newText)
                fileNew.close()


                fileNew=open("%s_extramodels.dat"%(name),"w+")
                text="""ssmzp_nonuniversal_v4.zip
"""
                if i_gl==0:
                        text="""ssmzp_nonuniversal_no_light_v4.zip
"""
                fileNew.write(text)
                fileNew.close()

                d2= dict(
                    NAME="%s"%(name),
                    SEED='%d'%(random.randint(0,100000)),
                )

                file=open("run_card.template","r")
                text=file.read()
                file.close()
                newText=Template(text).safe_substitute(d2)
                fileNew=open("%s_run_card.dat"%(name),"w+")
                fileNew.write(newText)
                fileNew.close()

