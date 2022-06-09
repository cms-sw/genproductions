#!/bin/env python
from math import pi,sqrt

Gf=1.1663787e-5
#1.1663787e-5
mW=80.385
g2=8.*mW**2. *Gf / sqrt(2.)

mb=4.700000
mt=173.20
gammaW=2.085


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
        print q1
        return 0
    if q2>3:
        print q2
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


