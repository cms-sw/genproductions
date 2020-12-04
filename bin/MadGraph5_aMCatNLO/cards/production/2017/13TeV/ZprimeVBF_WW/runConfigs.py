#!/bin/env python

import shutil
from string import Template
import random
import subprocess
import glob
# import wprimeWidth
import zprimeWidth
from math import sqrt,pi
import numpy as np


kv=[0.1,0.25,0.5,0.75,1.]
gl=[0.,1.]
gh=[1.]
#zprimeMasses=[500.,800.,1000.,2000.,3000.,4000.]
zprimeMasses=[250., 500., 750., 1000., 1250., 1500., 1750., 2000., 2250., 2500.]

mw=80.2673592

sinthetaW=0.23126
cos2thetaW=1.-sinthetaW


mZ=91.1876
mw=sqrt( cos2thetaW* mZ**2 )


for i_mzp in zprimeMasses:
    for i_gl in gl:
        for i_gh in gh:
            for i_kv in kv:

                name=("Zprime_WW_M_%s_gl_%g_gh_%g_kv_%s"%(i_mzp,i_gl,i_gh,i_kv)).replace(".","p")
                print("generating %s"%name)


                gzpvv=i_kv*(5.3*mw/i_mzp)**2.
                ch=1.
                width=zprimeWidth.zprimeTotal_pdg(i_mzp,i_gl,i_gh,gzpvv,ch)
                d = dict(
                    MZPRIME='%se+00'%(i_mzp),
                    WZPRIME='%g'%(width),
                    GL='%g'%(i_gl),
                    GH='%g'%(i_gh),
                    GZPVV='%g'%(gzpvv),
                )
                print( "mzp: {0}    mwp_width: {1}  gl: {2}    gh: {3}\n\n".format(i_mzp,width, i_gl, i_gh)  )

                file=open("param_card.template","r")
                text=file.read()
                file.close()
                newText=Template(text).safe_substitute(d)
                fileNew=open("%s_param_card.dat"%(name),"w+")
                fileNew.write(newText)
                fileNew.close()
                
                model="ssmzp_nonuniversal_v4"
                if i_gl==0:
                        model="ssmzp_nonuniversal_no_light_v4"
                d = dict(
                    modelfile="%s"%(),
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

