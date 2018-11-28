import os, math

class quark(object):
    def __init__(self,id,mass):
        self.id = id
        self.mass = mass
        self.massrun = mass
        self.bf = 1
        self.on = True
        self.active = True # for running nf

    def __repr__(self):
        return str(self.id)+": m = "+str(self.mass)+", mr = "+str(self.massrun)+", on = "+str(self.on)+", bf = "+str(self.bf)

# follows Ellis, Stirling, Webber calculations
class massRunner(object):
    def __init__(self):
        # QCD scale in GeV
        self.Lambda = 0.218

    # RG terms, assuming nc = 3 (QCD)
    def c(self): return 1./math.pi
    def cp(self,nf): return (303.-10.*nf)/(72.*math.pi)
    def b(self,nf): return (33.-2.*nf)/(12.*math.pi)
    def bp(self,nf): return (153.-19.*nf)/(2.*math.pi*(33.-2.*nf))
    def alphaS(self,Q,nf): return 1./(self.b(nf)*math.log(Q**2/self.Lambda**2))

    # derived terms
    def cb(self,nf): return 12./(33.-2.*nf)
    def one_c_cp_bp_b(self,nf): return 1.+self.cb(nf)*(self.cp(nf)-self.bp(nf))

    # constant of normalization
    def mhat(self,mq,nfq):
        return mq/math.pow(self.alphaS(mq,nfq),self.cb(nfq))/self.one_c_cp_bp_b(nfq)

    # mass formula
    def m(self,mq,nfq,Q,nf):
        # temporary hack: exclude quarks w/ mq < Lambda
        alphaq = self.alphaS(mq,nfq)
        if alphaq < 0: return 0
        else: return self.mhat(mq,nfq)*math.pow(self.alphaS(Q,nf),self.cb(nf))*self.one_c_cp_bp_b(nf)

    # operation
    def run(self,quark,nfq,scale,nf):
        # run to specified scale and nf
        return self.m(quark.mass,nfq,scale,nf)

class quarklist(object):
    def __init__(self):
        # mass-ordered
        self.qlist = [
            quark(2,0.0023), # up
            quark(1,0.0048), # down
            quark(3,0.095),  # strange
            quark(4,1.275),  # charm
            quark(5,4.18),   # bottom
        ]
        self.scale = None
        self.runner = massRunner()

    def set(self,scale):
        self.scale = scale
        # mask quarks above scale
        for q in self.qlist:
            # for decays
            if scale is None or 2*q.mass < scale: q.on = True
            else: q.on = False
            # for nf running
            if scale is None or q.mass < scale: q.active = True
            else: q.active = False
        # compute running masses
        if scale is not None:
            qtmp = self.get(active=True)
            nf = len(qtmp)
            for iq,q in enumerate(qtmp):
                q.massrun = self.runner.run(q,iq,scale,nf)
        # or undo running
        else:
            for q in self.qlist:
                q.massrun = q.mass

    def reset(self):
        self.set(None)

    def get(self,active=False):
        return [q for q in self.qlist if (q.active if active else q.on)]

class svjHelper(object):
    def __init__(self):
        self.xsecs = {
            500: 71.37, 600: 33.22, 700: 18.17, 800: 10.75, 900: 7.04, 1000: 4.612,
            1100: 3.021, 1200: 2.039, 1300: 1.459, 1400: 1.057, 1500: 0.77, 1600: 0.5667, 1700: 0.4327, 1800: 0.3304, 1900: 0.2479, 2000: 0.1849,
            2100: 0.1384, 2200: 0.1044, 2300: 0.08124, 2400: 0.06361, 2500: 0.04977, 2600: 0.03891, 2700: 0.03042, 2800: 0.02447, 2900: 0.01969, 3000: 0.0155,
            3100: 0.01209, 3200: 0.009518, 3300: 0.007494, 3400: 0.006148, 3500: 0.005036, 3600: 0.004047, 3700: 0.003252, 3800: 0.002613, 3900: 0.0021, 4000: 0.001688
        }
        self.quarks = quarklist()
        self.alphaName = ""
        # parameters for lambda/alpha calculations
        self.n_c = 2
        self.n_f = 2
        self.b0 = 11.0/6.0*self.n_c - 2.0/6.0*self.n_f

    def setAlpha(self,alpha):
        self.alphaName = alpha
        # "empirical" formula
        lambda_peak = 3.2*math.pow(self.mDark,0.8)
        if self.alphaName=="peak":
            self.alpha = self.calcAlpha(lambda_peak)
        elif self.alphaName=="high":
            self.alpha = 1.5*self.calcAlpha(lambda_peak)
        elif self.alphaName=="low":
            self.alpha = 0.5*self.calcAlpha(lambda_peak)
        else:
            raise ValueError("unknown alpha request: "+alpha)

    # calculation of lambda to give desired alpha
    # see 1707.05326 fig2 for the equation: alpha = pi/(b * log(1 TeV / lambda)), b = 11/6*n_c - 2/6*n_f
    # n_c = HiddenValley:Ngauge, n_f = HiddenValley:nFlav
    # see also TimeShower.cc in Pythia8, PDG chapter 9 (Quantum chromodynamics), etc.

    def calcAlpha(self,lambdaHV):
        return math.pi/(self.b0*math.log(1000/lambdaHV))

    def calcLambda(self,alpha):
        return 1000*math.exp(-math.pi/(self.b0*alpha))

    # has to be "lambdaHV" because "lambda" is a keyword
    def setModel(self,mZprime,mDark,rinv,alpha,lambdaHV=None):
        # store the basic parameters
        self.mZprime = mZprime
        self.mDark = mDark
        self.rinv = rinv
        if isinstance(alpha,str) and alpha[0].isalpha(): self.setAlpha(alpha)
        else: self.alpha = float(alpha)

        # get more parameters
        self.xsec = self.getPythiaXsec(self.mZprime)
        self.mMin = self.mZprime-1
        self.mMax = self.mZprime+1
        self.mSqua = self.mDark/2. # dark scalar quark mass (also used for pTminFSR)

        # get limited set of quarks for decays (check mDark against quark masses, compute running)
        self.quarks.set(mDark)

        if lambdaHV is not None:
            self.lambdaHV = lambdaHV
            self.alpha = self.calcAlpha(self.lambdaHV)
        else:
            self.lambdaHV = self.calcLambda(self.alpha)

    def getOutName(self,events,signal=True,outpre="outpre",part=None):
        _outname = outpre
        if signal:
            _outname += "_mZprime-{:g}".format(self.mZprime)
            _outname += "_mDark-{:g}".format(self.mDark)
            _outname += "_rinv-{:g}".format(self.rinv)
            if len(self.alphaName)>0: _outname += "_alpha-"+(self.alphaName)
            else: _outname += "_alpha-{:g}".format(self.alpha)
        _outname += "_n-{:g}".format(events)
        if part is not None:
            _outname += "_part-{:g}".format(part)
        return _outname

    # allow access to all xsecs
    def getPythiaXsec(self,mZprime):
        xsec = 1.0 
        # a function of mZprime
        if mZprime in self.xsecs: xsec = self.xsecs[mZprime]
        return xsec

    def invisibleDecay(self,mesonID,dmID):
        lines = ['{:d}:oneChannel = 1 {:g} 0 {:d} -{:d}'.format(mesonID,self.rinv,dmID,dmID)]
        return lines

    def visibleDecay(self,type,mesonID,dmID):
        theQuarks = self.quarks.get()
        if type=="simple":
            # just pick down quarks
            theQuarks = [q for q in theQuarks if q.id==1]
            theQuarks[0].bf = (1.0-self.rinv)
        elif type=="democratic":
            bfQuarks = (1.0-self.rinv)/float(len(theQuarks))
            for iq,q in enumerate(theQuarks):
                theQuarks[iq].bf = bfQuarks
        elif type=="massInsertion":
            denom = sum([q.massrun**2 for q in theQuarks])
            # hack for really low masses
            if denom==0.: return self.visibleDecay("democratic",mesonID,dmID)
            for q in theQuarks:
                q.bf = (1.0-self.rinv)*(q.massrun**2)/denom
        else:
            raise ValueError("unknown visible decay type: "+type)
        lines = ['{:d}:addChannel = 1 {:g} 91 {:d} -{:d}'.format(mesonID,q.bf,q.id,q.id) for q in theQuarks if q.bf>0]
        return lines

    def getPythiaSettings(self):
        # todo: include safety/sanity checks
        
        lines = [
            'HiddenValley:ffbar2Zv = on',
            # parameters for leptophobic Z'
            '4900023:m0 = {:g}'.format(self.mZprime),
            '4900023:mMin = {:g}'.format(self.mMin),
            '4900023:mMax = {:g}'.format(self.mMax),
            '4900023:mWidth = 0.01',
            '4900023:oneChannel = 1 0.982 102 4900101 -4900101',
            # SM quark couplings needed to produce Zprime from pp initial state
            '4900023:addChannel = 1 0.003 102 1 -1',
            '4900023:addChannel = 1 0.003 102 2 -2',
            '4900023:addChannel = 1 0.003 102 3 -3',
            '4900023:addChannel = 1 0.003 102 4 -4',
            '4900023:addChannel = 1 0.003 102 5 -5',
            '4900023:addChannel = 1 0.003 102 6 -6',
            # hidden spectrum:
            # fermionic dark quark,
            # diagonal pseudoscalar meson, off-diagonal pseudoscalar meson, DM stand-in particle,
            # diagonal vector meson, off-diagonal vector meson, DM stand-in particle
            '4900101:m0 = {:g}'.format(self.mSqua),
            '4900111:m0 = {:g}'.format(self.mDark),
            '4900211:m0 = {:g}'.format(self.mDark),
            '51:m0 = 0.0',
            '51:isResonance = false',
            '4900113:m0 = {:g}'.format(self.mDark),
            '4900213:m0 = {:g}'.format(self.mDark),
            '53:m0 = 0.0',
            '53:isResonance = false',
            # other HV params
            'HiddenValley:Ngauge = {:d}'.format(self.n_c),
            # when Fv has spin 0, qv spin fixed at 1/2
            'HiddenValley:spinFv = 0',
            'HiddenValley:FSR = on',
            'HiddenValley:fragment = on',
            'HiddenValley:alphaOrder = 1',
            'HiddenValley:Lambda = {:g}'.format(self.lambdaHV),
            'HiddenValley:nFlav = {:d}'.format(self.n_f),
            'HiddenValley:probVector = 0.75',
            # decouple
            '4900001:m0 = 5000',
            '4900002:m0 = 5000',
            '4900003:m0 = 5000',
            '4900004:m0 = 5000',
            '4900005:m0 = 5000',
            '4900006:m0 = 5000',
            '4900011:m0 = 5000',
            '4900012:m0 = 5000',
            '4900013:m0 = 5000',
            '4900014:m0 = 5000',
            '4900015:m0 = 5000',
            '4900016:m0 = 5000',
        ]
        # branching - effective rinv (applies to all meson species b/c n_f >= 2)
        # pseudoscalars have mass insertion decay, vectors have democratic decay
        lines += self.invisibleDecay(4900111,51)
        lines += self.visibleDecay("massInsertion",4900111,51)
        lines += self.invisibleDecay(4900211,51)
        lines += self.visibleDecay("massInsertion",4900211,51)
        lines += self.invisibleDecay(4900113,53)
        lines += self.visibleDecay("democratic",4900113,53)
        lines += self.invisibleDecay(4900213,53)
        lines += self.visibleDecay("democratic",4900213,53)

        return lines

