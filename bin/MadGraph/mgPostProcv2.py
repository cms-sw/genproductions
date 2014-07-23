###This script is the v2 of the post-Processing python tool
# 20/5/2011 - Alexis Kalogeropoulos - Added the -j == max jet flavor switch - If is set to 4, then you apply cuts on the b's, while if is set to 5 you do not
# 14/5/2011 - Alexis Kalogeropoulos - Add the seed information, nqmatch and number of events in the banner information -- Pythia fragments from CMSSW > 41x can use MEMAIN_nqmatch=('-1') and read this value directly from the LHE file

#!/usr/bin/env python
import sys, math, getopt
from copy import deepcopy
import pprint

try:
	enumerate = enumerate
except:
	# stupid python 2.2 doesn't have a builtin enumerator
	def enumerate(iterable):
		i = 0
		for item in iterable:
			yield (i, item)
			i += 1

#lepton masses from PDGLive
massDict = {
	11: 0.000510998910,
	13: 0.1056583668,
	15: 1.77684,
         5: 4.80000,
         4: 1.50000
}
tolerance = 1.0e-4

class FiveVector:
	def __init__(self, px, py, pz, m, m_ = None):
		if m_ is not None:
			self.vec = [float(px), float(py), float(pz),
			            float(m), float(m_)]
			return
		m2 = m**2
		if m < 0:
			m2 = -m2
		e = math.sqrt(px**2 + py**2 + pz**2 + m2)
		self.vec = [float(px), float(py), float(pz), e, float(m)]

	def p2(self):
		return reduce(lambda x, y: x + y**2, self.vec[:3], 0.0)

	def p(self):
		return math.sqrt(self.p2())

	def pt(self):
		return math.sqrt(self.vec[0]**2 + self.vec[1]**2)

	def e(self):
		return self.vec[3]

	def m(self):
		return self.vec[4]

	def _m(self):
		global tolerance

		m2 = self.vec[3]**2 - self.p2()
		if m2 < 0:
			m = -math.sqrt(-m2)
		else:
			m = math.sqrt(m2)
		if abs(m) < tolerance:
			return 0
		return m

	def __add__(self, other):
		result = deepcopy(self)
		for i in range(4):
			result.vec[i] += other.vec[i]
		result.vec[4] = result._m()
		return result

	def __sub__(self, other):
		result = deepcopy(self)
		for i in range(4):
			result.vec[i] -= other.vec[i]
		result.vec[4] = result._m()
		result.status = 2
		return result

	def __repr__(self):
		return '(%f, %f, %f, %f, %f)' % tuple(self.vec)

	def boost(self, ref, rdir):
		pmag = ref.vec[3]
		db = map(lambda x: x * rdir / pmag, ref.vec[:3])
		DB2 = reduce(lambda x, y: x + y**2, db, 0.)
		DB = math.sqrt(DB2)
		DGA = 1.0 / math.sqrt(1.0 - DB2)
		DBP = reduce(lambda x, y: x + y,
		             [db[i] * self.vec[i] for i in range(3)])
		DGABP = DGA * (DGA * DBP / (1.0 + DGA) + self.vec[3])
		for i in range(3):
			self.vec[i] += DGABP * db[i]
		self.vec[3] = DGA * (self.vec[3] + DBP)

	def reScale(self, pi, po):
		for i in range(3):
			self.vec[i] *= po / pi


class Particle:
	def __init__(self, *args):
		self.pdgId = args[0]
		self.cols = tuple(args[1:3])
		if len(args) == 4:
			self.p5 = FiveVector(0, 0, args[3], 0)
		else:
			self.p5 = FiveVector(*args[3:])
		self.mothers = (0, 0)
		self.status = 1

	def __add__(self, other):
		result = deepcopy(self)
		result.p5 += other.p5
		result.status = 2
		return result

	def __sub__(self, other):
		result = deepcopy(self)
		result.p5 -= other.p5
		result.status = 2
		return result

	def __repr__(self):
		return '(pdgId = %d, status = %d, mo = (%d, %d), ' \
		       'cols = (%d, %d), p5 = %s)' % \
			(self.pdgId, self.status,
			 self.mothers[0], self.mothers[1],
			 self.cols[0], self.cols[1], repr(self.p5))


def format(f, line):
	data = line.split()
	if len(data) != len(f):
		raise Exception("Expected %d values, got %d" %
							(len(f), len(data)))
	def convert(t, v):
		if t == 'i':
			return int(v)
		elif t == 'f':
			return float(v)
		else:
			raise Exception("This should never happen!")

	return map(lambda(i, x): convert(f[i], x), enumerate(data))


def fixMissingWinTop(particles):
	tops = [x + 1 for x, y in enumerate(particles) if abs(y.pdgId) == 6]
	newParticles = []
	Ws = {}
	mothersMap = { 0: 0 }
	for i, j in enumerate(particles):
		j = deepcopy(j)
		k = len(newParticles) + 1
		if j.mothers[0] in tops and abs(j.pdgId) in (1, 2, 3, 4, 11, 12, 13, 14, 15, 16):
			if not Ws.has_key(j.mothers[0]):
				p = Particle(newParticles[j.mothers[0] - 1].pdgId * 4, 0, 0, 0)
				p.mothers = j.mothers
				p.time = 0
				p.spin = 9
				newParticles.append(p)
				Ws[p.mothers[0]] = k
				k += 1

			W = Ws[j.mothers[0]]
			newParticles[W - 1] += j
			j.mothers = (W, W)
		else:
			j.mothers = (mothersMap[j.mothers[0]], mothersMap[j.mothers[1]])

		mothersMap[i + 1] = k

		newParticles.append(j)

	return newParticles


def fixMissingWZ(particles):
	orphans = []
	for i, p in enumerate(particles):
		if p.mothers[0] > 0 and \
		   particles[p.mothers[0] - 1].status < 0 and \
		   abs(p.pdgId) in range(11, 17):
			orphans.append(i)

	if len(orphans) % 2 != 0:
		print >> sys.stderr, "single orphan; do not know how to process"
		return particles

	while len(orphans):
		a = orphans[0]
		pa = particles[a]
		idMother = 0
		for b in orphans[1:]:
			pb = particles[b]
			lq = pa.pdgId + pb.pdgId
			if abs(lq) == 1:
				idMother = 24 * lq
				break
			elif lq == 0:
				idMother = 23
				break
		if idMother == 0:
			print >> sys.stderr, \
				"cannot find pair for pdgId %d" % pa.pdgId
			return particles

		orphans = filter(lambda x: x not in (a, b), orphans)
		p = deepcopy(pa)
		p += pb
		p.time = 0
		p.spin = 9
		p.status = 2
		p.pdgId = idMother

		orphans = map(lambda x: x + (x > a), orphans)
		for q in particles:
			mo = list(q.mothers)
			for i, j in enumerate(mo):
				if j > a:
					mo[i] += 1
			q.mothers = tuple(mo)
		particles.insert(a, p)
		pa.mothers = (a + 1, a + 1)
		pb.mothers = (a + 1, a + 1)

	return particles


def fixMasses(particles):
	global massDict, tolerance

	class Node:
		def __init__(self, index, particle):
			self.index = index
			self.particle = particle
			self.mothers = []
			self.daughters = []

		def connect(self, nodes):
			for i in self.particle.mothers:
				if i < 1:
					continue
				if nodes[i - 1] not in self.mothers:
					self.mothers.append(nodes[i - 1])
				if self not in nodes[i - 1].daughters:
					nodes[i - 1].daughters.append(self)

		def boost(self, vec):
			self.particle.p5.boost(vec, +1)
			for i in self.daughters:
				i.boost(vec)

		def __repr__(self):
			return repr({ 'mothers': [ x.index for x in self.mothers ], 'daughters': [ x.index for x in self.daughters ], 'index': self.index, 'id': self.particle.pdgId })

	nodes = [ Node(i, p) for i, p in enumerate(particles) ]
	for i in nodes:
		i.connect(nodes)

	needsFixing = []
	for i in nodes:
		if i.particle.status != 1:
			continue
		absId = abs(i.particle.pdgId)
		if absId not in massDict:
			continue
		m = i.particle.p5.vec[4]
		if abs(massDict[absId] - m) < tolerance:
			continue
		needsFixing.append(i.index)

	while len(needsFixing):
		n = nodes[needsFixing.pop()]
		if len(n.mothers) >= 1 and n.mothers[0].particle.status < 1:
			ref = FiveVector(0.0,0.0,0.0,0.0)
			# Define boost to rest frame
			for ip,p in enumerate(particles):
				if p.status == -1: continue
				if particles[p.mothers[0]-1].status < 1:
					ref += p.p5
					
			roots = ref._m()
			ref.vec[4] = roots
			# The recoil does NOT include the particle to be fixed
			prec = ref - n.particle.p5
			mrec = prec._m()
			prec.vec[4] = mrec

			# Boost a copy of the particle momentum to the rest frame
			ptmp = deepcopy(n.particle.p5)
			ptmp.boost(ref, -1)
			rsh = roots
			# new mass, energy, momentum
			mnew=massDict[abs(n.particle.pdgId)]
			ptmp.vec[3] = (rsh**2 + mnew**2 - mrec**2) \
			              / (2.0 * rsh)

			pmagOld = ptmp.p()
			pmagNew = math.sqrt(max(0.,
					ptmp.e()**2 - mnew**2))
			ptmp.reScale(pmagOld, pmagNew)
			ptmp.vec[4] = ptmp._m()

			# update recoil energy in CMS
			erecs = rsh - ptmp.e()
			# boost corrected parton back to lab frame
			ptmp.boost(ref, +1)
			n.particle.p5 = ptmp

			# Find new momentum to match energy of sum recoil
			pxNew = erecs**2 - mrec**2
			if pxNew>0:
				pxNew=math.sqrt(pxNew)
			else:
				pxNew=0
				print("problem with recoil kinematics\n")

			prec.boost(ref, -1)
			psave = deepcopy(prec)
			pxOld = prec.p()
			prec.reScale(pxOld,pxNew)
			prec.vec[3] = math.sqrt(prec.p2()+mrec**2)

			# Determine boost to new rest frame (velocity addition rule)
			pbop = deepcopy(prec)
			for i in range(0,3):
				pbop.vec[i] -= psave.vec[i]*prec.vec[3]/psave.vec[3]

			pbop.vec[3] -= pxOld*pxNew/psave.vec[3]

			# Fix individual components of the recoil
			for ip,p in enumerate(particles):
				if p.status == -1: continue
				if ip == n.index: continue
				p.p5.boost(ref, -1)
				p.p5.boost(pbop, +1)
				p.p5.boost(ref, +1)

		elif len(n.mothers) == 1:
			p = n.particle
			mo = n.mothers[0].particle
			dau = n.mothers[0].daughters
			dau = filter(lambda x: x.index != n.index, dau)
			if not len(dau):
				print >> sys.stderr, "WTF"
				continue

			if len(dau) > 1:
				o = deepcopy(dau[0].particle)
				for j in dau[1:]:
					o += j.particle
				m = Node(0, o)
				m.daughters = dau
			else:
				m = dau[0]
				o = m.particle
				if m.index in needsFixing:
					o.p5.vec[4] = massDict[abs(o.pdgId)]
					needsFixing = filter(
						lambda x: x != m.index,
						needsFixing)

			p.p5.vec[4] = massDict[abs(p.pdgId)]
			orig = deepcopy(o)

			p.p5.boost(mo.p5, -1)
			o.p5.boost(mo.p5, -1)

			rsh = mo.p5.m()
			p.p5.vec[3] = (rsh**2 + p.p5.m()**2 - o.p5.m()**2) \
			              / (2.0 * rsh)
			pmagOld = p.p5.p()
			pmagNew = math.sqrt(max(0.,
					p.p5.e()**2 - p.p5.m()**2))
			p.p5.reScale(pmagOld, pmagNew)
			o.p5 = FiveVector(0, 0, 0, rsh, rsh) - p.p5

			p.p5.boost(mo.p5, +1)
			o.p5.boost(mo.p5, +1)

			if len(m.daughters):
				boost = o.p5 - orig.p5
				boost.vec[3] = o.p5.vec[3] + orig.p5.vec[3]
				boost.boost(boost, +1)
				for i in m.daughters:
					i.boost(boost)

		else:
			print >> sys.stderr, \
				"Don't know how to fix particle at %d" % i.index

	return particles

def getEvent(f, g = None, headerXfrm = None):
	headers = None
	while True:
		while True:
			line = f.readline()
			if headers is not None:
				headers.append(line)
				if line.find('</header') >= 0:
					headers = headerXfrm(headers)
					if g is not None:
						print >> g, \
							reduce(lambda x, y:
									x + y,
							       headers),
					headers = None
			elif not len(line):
				break
			elif line.find('<event') >= 0:
				break
			elif headerXfrm is not None:
				if line.find('<header') >= 0:
					headers = [ line ]
				else:
					print >> g, line,
			elif g is not None:
				print >> g, line,

		if not len(line):
			break

		if g is not None:
			print >> g, line,
		line = f.readline()
		nup, idprup, wgt, scale, aqedup, aqcdup = format('iiffff', line)

		particles = []
		comments = []
		
		while True:
			line = f.readline()
			if line.find('<clus') >= 0:
				break
			if line.find('</clus') >= 0:
				break
			
			if line.find('</event') >= 0:
				break

			if line[0] == '#':
				comments.append(line)
				continue
			
			id, status, mo1, mo2, col1, col2, px, py, pz, e, m, time, spin = format('iiiiiifffffff', line)
			p = Particle(id, col1, col2, px, py, pz, e, m)
			p.status = status
			p.mothers = (mo1, mo2)
			p.time = time
			p.spin = spin
			particles.append(p)

		yield (idprup, wgt, scale, aqedup, aqcdup, particles)

		if g is not None:
			if len(comments) > 0:
				print >> g, reduce(lambda x, y: x + y, comments), line,
		        elif len(comments) == 0:
				print >> g, line,

				
def writeEvent(g, idprup, wgt, scale, aqedup, aqcdup, particles):
	print >> g, ' %d %d %14.7E %14.7E %14.7E %14.7E' % \
			(len(particles), idprup, wgt, scale, aqedup, aqcdup)

	for p in particles:
		print >> g, '%8d %4d %4d %4d %4d %4d' \
		      ' %18.11E %18.11E %18.11E %18.11E %18.11E %1.4E %4.1f' % \
		      (p.pdgId, p.status, p.mothers[0], p.mothers[1],
		       p.cols[0], p.cols[1], p.p5.vec[0],
		       p.p5.vec[1], p.p5.vec[2], p.p5.vec[3],
		       p.p5.vec[4], p.time, p.spin)


MGParamCMSDescr = {
	#'nevts': 	'Number of events',
	'minjets': 	'Smallest number of additional light flavour jets',
	'maxjets': 	'Largest number (inclusive ktMLM matching multipl.)',
	'etaclmax':	'Maximum pseudorapidity for particles to cluster',
	'qcut': 	'Jet matching threshold for ktMLM scheme',
	'nqmatch': 	'Max Jet Flavor'
	#'seed': 	'Random seed number'
}

def headerXfrm(headers, nEvents, minJets, maxJets,addn = {}, strip = False):
	global MGParamCMSDescr
	newHeaders = []
	header = None
	mgParams = {}
	gridParams = {}
	drop = False
	for line in headers:
		if len(line) > 2 and line[0] == '<':
			if line[1] == '/':
				header = None
				if drop:
					drop = False
					continue
			else:
				p = 1
				while p < len(line) and \
				      line[p] not in " \t\r\n>":
					p += 1
				header = line[1:p]
		if drop:
			continue

		if header in ('MGRunCard', 'MGGridCard'):
			params = []
			for i in line.split():
				while True:
					p = i.find('=')
					if p < 0:
						break
					if p > 0:
						params.append(i[:p])
					params.append('=')
					i = i[p + 1:]
				p = i.find('!')
				if p > 0:
					params.append(i[:p])
					i = i[p:]
				if len(i):
					params.append(i)

			if len(params) >= 3 and \
			   params[0][0] != '#' and \
			   (len(params) == 3 or params[3][0] == '!') and \
			   params[1] == '=':
				if header == 'MGGridCard':
					gridParams[params[2]] = params[0]
				else:
					mgParams[params[2]] = params[0]
				if params[2] == 'iseed' and strip:
					tmp = "      0       = iseed   "
					if line.find('!') >= 0:
						tmp += line[line.find('!'):]
					line = tmp.rstrip() + '\n'
				elif params[2] == 'gseed' and strip:
					tmp =  "  0    = gseed   "
					if line.find('!') >= 0:
						tmp += line[line.find('!'):]
					line = tmp.rstrip() + '\n'
				if params[2] == 'gevents' and strip:
					tmp = " 0      = gevents "
					if line.find('!') >= 0:
						tmp += line[line.find('!'):]
					line = tmp.rstrip() + '\n'
				if params[2] == 'nevents' and strip:
					tmp = "      0       = nevents "
					if line.find('!') >= 0:
						tmp += line[line.find('!'):]
					line = tmp.rstrip() + '\n'

					
		elif header in ('MGGenerationInfo', 'ReplaceParticleInfo'):
			if strip:
				drop = True
				continue
		elif header == 'MGDecayInfo':
			if line[0] == '#':
				tmp = "".join(map(lambda x: x.lower(), line))
				skip = False
				for i in ('rnd seed', 'events', 'weight', 'wgt', 'partial width'):
					if tmp.find(i) >= 0:
						skip = False
						break
				if skip:
					continue

		newHeaders.append(line)

	if 'ickkw' in mgParams:
	#	if int(mgParams['ickkw']) == 1:
			#if 'nevts' not in addn:
			#	addn['nevts'] = nEvents
			if 'minjets' not in addn:
				addn['minjets'] = minJets
			if 'maxjets' not in addn:
				addn['maxjets'] = maxJets
	
	if 'maxjetflavor' in mgParams:
		if 'nqmatch' not in addn:
			addn['nqmatch'] = int(mgParams['maxjetflavor'])

	#if 'gseed' in gridParams:
	#	if 'seed' not in addn:
	#	   addn['seed'] = int(gridParams['gseed'])


	if len(addn):
		lines = map(lambda x: x + '\n', """<MGParamCMS>
# All parameters that are given here can be selected to be set from this
# header by setting the # corresponding CMSSW config parameter to -1
# In case this is done, the entries here must exist of an error message
# is given.""".split('\n'))
		for i, j in addn.items():
			line = "%12s = %-10s" % (repr(j), i)
			if i in MGParamCMSDescr:
				line += " ! %s" % MGParamCMSDescr[i]
			lines.append(line + '\n')
		lines.append('</MGParamCMS>\n')

		newHeaders[-1:-1] = lines

	return newHeaders


def syntax(out):
	out.write("Syntax: %s [OPTIONS] <input LHE file>\n\n"
	          "    Options:\n"
	          "\t-h, --help             Show this help\n"
	          "\t-o, --output=<file>    LHE output file name\n"
	          "\t-q, --qcut=<value>     Value for jet matching threshold\n"
	          "\t-e, --etaclmax=<value> Value for jet clustering limit\n"
	          "\t-w, --wz               Add missing W/Z in lepton orphans\n"
	          "\t-t, --topw             Add W resonance in top decays\n"
	          "\t-m, --masses           Fix lepton masses\n"
	          "\t-s, --strip            Clean headers for LHE merging\n"
	          "\t-j, --maxjetflavor=<value>     Define the maxjetflavor\n"
	          "\n" % sys.argv[0])

def main(args):
	longOptions = [ 'help', 'output', 'qcut', 'etaclmax', 'wz', 'topw', 'masses', 'strip','maxjetflavor']
	shortOptions = 'ho:q:e:j:wtms'

	addn = {}
	wz = False
	topw = False
	masses = False
	strip = False
	mjf=0
	g = sys.stdout

	try:
		opts, args = getopt.getopt(args, shortOptions, longOptions)
        except getopt.GetoptError:
		# fail if an invalid option or missing argument was found
		syntax(sys.stderr)
		return 1

	for opt, arg in opts:
		if opt in ('-h', '--help'):
			syntax(sys.stdout)
		elif opt in ('-q', '--qcut'):
			addn['qcut'] = float(arg)
		elif opt in ('-e', '--etaclmax'):
			addn['etaclmax'] = float(arg)
		elif opt in ('-w', '--wz'):
			wz = True
		elif opt in ('-t', '--topw'):
			topw = True
		elif opt in ('-m', '--masses'):
			masses = True
		elif opt in ('-s', '--strip'):
			strip = True
		elif opt in ('-j', '--maxjetflavor'):
			mjf = int(arg)
		elif opt in ('-o', '--output'):
			g = open(arg, 'w')
			#addn['MaxJetFlavor'] = int(arg)

	
	if len(args) != 1:
		syntax(sys.stderr)
		return 1

	f = open(args[0], 'r')

	minJets = -1
	maxJets = -1
	nEvents = 0
	nevts = -1
	MaxJetFlavor=mjf
	#MaxJetFlavor=
	# FIXME: Matching code uses c and b as light jets
	#        However, there is also a "maxjetflavour" parameter to MG...
	if MaxJetFlavor ==5:
		lightJets = dict([(x, True) for x in \
	                 	(-1, 1, -2, 2, -3, 3, -4, 4, -5, 5, 21)])
	elif MaxJetFlavor ==4:
		lightJets = dict([(x, True) for x in \
	                 	(-1, 1, -2, 2, -3, 3, -4, 4,  21)])
		
        if MaxJetFlavor >3: 
		for idprup, wgt, scale, aqedup, aqcdup, particles in getEvent(f):
			nEvents += 1
			beams = []
			nlJets = 0
			for i, p in enumerate(particles):
				if p.status < 0:
					beams.append(i + 1)
					if p.mothers[0] in beams:
						if p.pdgId in lightJets:
							nlJets += 1
							if minJets < 0 or nlJets < minJets:
								minJets = nlJets
								if maxJets < 0 or nlJets > maxJets:
									maxJets = nlJets
									
									nevts=nEvents
									
									f.seek(0, 0)
									
	def xfrm(headers):
		return headerXfrm(headers, nEvents, minJets, maxJets, addn, strip)

	for idprup, wgt, scale, aqedup, aqcdup, particles in \
						getEvent(f, g, xfrm):
		if topw:
			particles = fixMissingWinTop(particles)
		if wz:
			particles = fixMissingWZ(particles)
		if masses:
			particles = fixMasses(particles)

		writeEvent(g, idprup, wgt, scale, aqedup, aqcdup, particles)

	f.close()

	return 0

if __name__ == '__main__':
	sys.exit(main(sys.argv[1:]))
