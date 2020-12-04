#!/bin/env python
from math import pi,sqrt
import numpy as np

Gf=1.1663787e-5
#1.1663787e-5
#mW=80.385
mZ=91.1876

sin2tw=0.23126
#0.231265
#sin2tw=0.234
cos2tw=1.-sin2tw
#mW=80.403
mW=sqrt( cos2tw* mZ**2 )

tantw=sqrt(sin2tw/cos2tw)

g2=8.*mW**2. *Gf / sqrt(2.)

mb=4.700000
mt=173.20
gammaW=2.085


def zprimefermion_single(mzp,cf,gv):
    #from 1402.4431 HVT model
    return 1./2.*(g2*cf/gv)**2 *mzp/(48.*pi)
    
    
def zprimefermion_single_pdg(mzp,cf,gv,Qf,T3):
    #from pdg z width
    #return sqrt(2)*cf*Gf*mzp**3/(6.*pi)*((T3-Qf*sin2tw)**2+(Qf*sqrt(sin2tw))**2)
    
    cV=T3-2.*sin2tw*Qf
    cA=T3
    #print("fermion:  ",Qf,T3,cV,cA)
    #return cf/gv*g2/(48.*pi* cos2tw)*mzp*(cV**2+cA**2)
    return cf/gv*g2/(48.*pi* cos2tw)*mzp*(cV**2+cA**2)
    
def zprimeallferions_pdg(mzp,cf_l,cf_h,gv):
    #from 1402.4431 HVT model
    Nc=3.
    #l_quarks=Nc*2.*(zprimefermion_single_pdg(mzp,cf_l,gv,-1./3.,-0.5)+zprimefermion_single_pdg(mzp,cf_l,gv,2./3.,0.5))
    l_up=(zprimefermion_single_pdg(mzp,cf_l,gv,2./3.,0.5))
    l_down=(zprimefermion_single_pdg(mzp,cf_l,gv,-1./3.,-0.5))
    
    
    #print("u quark: ",Nc*zprimefermion_single_pdg(mzp,cf_l,gv,2./3.,0.5))
    #print("d quark: ",Nc*zprimefermion_single_pdg(mzp,cf_l,gv,-1./3.,-0.5))
    #print("u qurak hand: ", Gf/(sqrt(2)*6*pi)*((0.5-4/3* sin2tw)**2+0.5**2)*mzp**3)
    #print("d quark: ",Nc*(zprimefermion_single_pdg(mzp,cf_l,gv,-1./3.,0.5)))
    #h_quarks=Nc*(zprimefermion_single_pdg(mzp,cf_l,gv,-1./3.,0.5)+zprimefermion_single_pdg(mzp,cf_l,gv,2./3.,-0.5))
    h_up=(zprimefermion_single_pdg(mzp,cf_h,gv,2./3.,0.5))
    h_down=(zprimefermion_single_pdg(mzp,cf_h,gv,-1./3.,-0.5))
    
    l_lep=(zprimefermion_single_pdg(mzp,cf_l,gv,-1.,-0.5))
    l_neut=(zprimefermion_single_pdg(mzp,cf_l,gv,0,0.5))
    
    #print("lepton:  ",zprimefermion_single_pdg(mzp,cf_l,gv,-1.,-0.5), "neutrino:  ",3.*zprimefermion_single_pdg(mzp,cf_l,gv,0,0.5))
    h_lep=(zprimefermion_single_pdg(mzp,cf_h,gv,-1.,-0.5))
    h_neut=(zprimefermion_single_pdg(mzp,cf_h,gv,0,0.5))
    #h_lep=zprimefermion_single_pdg(mzp,cf_h,gv,0,0.5)+zprimefermion_single_pdg(mzp,cf_h,gv,-1.,-0.5)
    
    print(("neutrino: ",l_neut,"  lepton:  ",l_lep, "  up:  ",l_up, "  down: ",l_down))
    return 2.*l_neut+h_neut+  2.*l_lep+h_lep   + 2.*Nc*l_up + Nc*h_up + 2.*Nc*l_down + Nc*h_down


def zprimeallferions(mzp,cf_l,cf_h,gv):
    #from 1402.4431 HVT model
    Nc=3.
    l_quarks=Nc*4.*zprimefermion_single(mzp,cf_l,gv)
    h_quarks=Nc*2.*zprimefermion_single(mzp,cf_h,gv)
    l_lep=4.*zprimefermion_single(mzp,cf_l,gv)
    h_lep=2.*zprimefermion_single(mzp,cf_h,gv)
    return l_quarks+h_quarks+l_lep+h_lep
    
def zprimeBoson(mzp,ch,gv):
    #from 1402.4431 HVT model
    #factor of 2 for Z'-> WW and Z'->Zh
    #gv<sqrt(0.5*192.*pi/(ch**2)
    #print( (192.*pi/cos2tw)**(1./4.)  )
    return (gv**2)*cos2tw*(mzp**5)/(mW**4)/(192.*pi) * (1.- 4.* mW**2/mzp**2)**(3./2.)* (1.+ 20. * (mW/mzp)**2 + 12.*(mW/mzp)**4)
    #return 2.*gv**2*ch**2*mzp/(192.*pi)
    
def zprimeTotal(mzp,cf_l,cf_h,gv,ch):
    return zprimeallferions(mzp,cf_l,cf_h,gv)+zprimeBoson(mzp,ch,gv)
    
def zprimeTotal_pdg(mzp,cf_l,cf_h,gv,ch):
    #return zprimeallferions_pdg(mzp,cf_l,cf_h,gv)
    #return zprimeBoson(mzp,ch,gv)
    return zprimeBoson(mzp,ch,gv)+ zprimeallferions_pdg(mzp,cf_l,cf_h,gv)
    #+zprimeBoson(mzp,ch,gv)

def wprimeTotal(mw):
    #from arxiv:hep-ph/0207290

    beta=1-mt**2/mw**2
    alpha=1+mt**2/(2*mw**2)
    #alpha=1.
    topPart=sum([ beta*alpha*CKM(3,i) for i in range(1,4,1)])
    lQuartPart=0.
    for i in range(1,3,1):
        for j in range(1,3,1):
            lQuartPart+=CKM(j,i)

    leptonPart=1.
    #return mw*(g2)/(48.*pi)  *( 3.*topPart+ 3.* lQuartPart + leptonPart    )
    return 3.*wprimelepton(mw)+wprimetb(mw)+wprimelight(mw)+wprimeHiggs(mw)
def wprimeTotalNoHiggs(mw,g=1.):
    #from arxiv:hep-ph/0207290

    beta=1-mt**2/mw**2
    alpha=1+mt**2/(2*mw**2)
    #alpha=1.
    topPart=sum([ beta*alpha*CKM(3,i) for i in range(1,4,1)])
    lQuartPart=0.
    for i in range(1,3,1):
        for j in range(1,3,1):
            lQuartPart+=CKM(j,i)

    leptonPart=1.
    #return mw*(g2)/(48.*pi)  *( 3.*topPart+ 3.* lQuartPart + leptonPart    )
    return 3.*g**2.*wprimelepton(mw)+wprimetb(mw)+wprimelight(mw)



def wprimetb(mw):
    beta=1-mt**2/mw**2
    alpha=1+mt**2/(2*mw**2)
    #alpha=1.
    #topPart=sum([ beta*alpha*CKM(3,i) for i in range(1,4,1)])
    topPart= beta*alpha
    return wprimelepton(mw) *( 3* topPart   )

def wprimelight(mw):
    lQuartPart=0.
    for i in range(1,4,1):
        for j in range(1,3,1):
            lQuartPart+=CKM(j,i)
    #return mw*(g2)/(16.*pi)  *(3.* lQuartPart   )
    return wprimelepton(mw)*6.

def wprimelepton(mw):
    return mw*(g2)/(48.*pi)

def wprimeHiggs(mw):
    return mw*(g2)/(48.*pi)/4.


def CKM(q1,q2):
    if q1>3:
        print(q1)
        return 0
    if q2>3:
        print(q2)
        return 0
    ckm=[   [0.97427, 0.22536, 0.00355],
            [0.22522,  0.97343, 0.0414 ],
            [0.00886,  0.0405,  0.99914]
        ]
    return ckm[q1-1][q2-1]

def F(x1,x2):
    return (2-x1*x1-x2*x2-(x1*x1-x2*x2)**2)*sqrt((1-(x1+x2)**2)*(1-(x1-x2)**2))

def SSMwidth(MWp):
    return MWp*(g2)/2./48./pi*(18.+3.*F(mt/MWp,mb/MWp))


def WprimeWidth(mWp,g1):
   m_t=173.20  #GeV
   m_b=4.70   #GeV
   g=0.65295357004
   return g1*g1*mWp*g*g/2/48/pi*(18+3*F(m_t/mWp,m_b/mWp))

def simpleWidth(mWp):
    leptonWidth=3.*sqrt(2)*Gf*(mW**2)*mWp/(12.*pi)
    #we need the mW because Gf=g^2 /8MW^2
    qarkwidth=0.
    for q1 in range(1,4):
        for q2 in range(1,4):
            qarkwidth+=3.*sqrt(2)*Gf*((CKM(q1,q2))**2)*((mW**2)*mWp)/(12.*pi)
    return leptonWidth+qarkwidth


