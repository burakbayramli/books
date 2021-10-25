## Copyright 2017 Timothy A. Handy
##
## Permission is hereby granted, free of charge, to any person obtaining
## a copy of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included
## in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
## OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

class initialState:
	def __init__(self, gamma_ideal, stateL, stateR):
		# Ideal gas gamma
		self.gamma = gamma_ideal

		# Left state
		self.densL = float(stateL[0])
		self.presL = float(stateL[2])
		self.velxL = float(stateL[1])

		# Right state
		self.densR = float(stateR[0])
		self.presR = float(stateR[2])
		self.velxR = float(stateR[1])

class exactRP(initialState):
	def __init__(self, gamma_ideal, stateL, stateR):
		from math import sqrt

		# Initialize states
		super(exactRP, self).__init__(gamma_ideal, stateL, stateR)
		self.cspdL = sqrt(self.gamma*self.presL/self.densL)
		self.cspdR = sqrt(self.gamma*self.presR/self.densR)

		# Useful variables (See Toro '99, page 153)
		self.G1 = 0.5e0*(self.gamma - 1.e0)/self.gamma
		self.G2 = 0.5e0*(self.gamma + 1.e0)/self.gamma
		self.G3 = 1.e0/self.G1
		self.G4 = 1.e0/(self.G1*self.gamma)
		self.G5 = 1.e0/(self.G2*self.gamma)
		self.G6 = self.G1/self.G2
		self.G7 = self.G1*self.gamma
		self.G8 = self.gamma - 1.e0

		# State in the star region
		self.presS = 0.e0
		self.velxS = 0.e0

		# Store whether .solve() was successful
		self.success = False

	def solve(self):
		self.success = True

		# 1) Check pressure positivity
		if(self.G4*(self.cspdL+self.cspdR)<=(self.velxR-self.velxL)):
			print('[exactRP::solve] Initial state will generate a vacuum. Exiting!')
			self.success = False
			return False

		# 2) Compute the pressure and velocity in the star region
		success = self.starpu()
		if(not success):
			print('[exactRP::solve] Unable to calculate pressure and velocity in star region!')
			self.success = False
			return False

		# 3) Exit as a success
		return True

	def starpu(self):
		from math import fabs

		# Outputs
		pstar = -1e99
		ustar = -1e99

		# Define solver criteria
		MAX_ITER = 100
		TOL_PRES = 1e-8

		# 1) Compute initial pressure guess and prepare for Newton-Raphson
		pstar0 = self.guessp()
		pold   = pstar0
		pcur   = pold
		deltau = self.velxR - self.velxL

		# 2) Perform Newton-Raphson iteration to determine pressure in
		#    the star region
		nrSuccess = False
		for it in range(1,MAX_ITER+1):
			fL, dfL = self.prefun(pold, self.densL, self.presL, self.cspdL)
			fR, dfR = self.prefun(pold, self.densR, self.presR, self.cspdR)
			pcur = pold - (fL + fR + deltau)/(dfL + dfR)
			dp = 2.e0*fabs((pcur - pold)/(pcur + pold))
			if(dp <= TOL_PRES):
				nrSuccess = True
				break
			if(pcur < 0.e0):
				pcur = TOL_PRES
			pold = pcur

		if(not nrSuccess):
			print('[starpu] Newton-Raphson unable to converge')
		else:
			pstar = pcur
			ustar = 0.5e0*(self.velxL + self.velxR + fR - fL)

		self.presS = pstar
		self.velxS = ustar
		return nrSuccess


	def guessp(self):
		from math import pow, sqrt

		# Outputs
		pm = -1e99

		# Define user-chosen pressure ratio, below which we consider
		# use of the pressure estimate from the Primitive Variable Riemann Solver
		quser = 2.e0

		# 1) Obtain initial guess using the primitive variable Riemann solver
		cup  = 0.25e0*(self.densL + self.densR)*(self.cspdL + self.cspdR)
		ppv  = 0.5e0*(self.presL + self.presR) + 0.5e0*(self.velxL - self.velxR)*cup
		ppv  = max([0.e0, ppv])
		pmin = min(self.presL, self.presR)
		pmax = max(self.presL, self.presR)
		qmax = pmax/pmin

		# 2) Decide whether to use the PVRS value, the Two-Rarefaction value,
		#    or the Two-Shock value
		if( (qmax <= quser) and (pmin <= ppv) and (ppv <= pmax) ):
			# Use the PVRS pressure value
			pm = ppv
		else:
			if(ppv < pmin):
				# Use the Two-Rarefaction Riemann solver
				pq = pow(self.presL/self.presR, self.G1)
				um = (pq*self.velxL/self.cspdL + self.velxR/self.cspdR \
					  + self.G4*(pq - 1.e0))/(pq/self.cspdL + 1.e0/self.cspdR)
				ptL = 1.e0 + self.G7*(self.velxL - um)/self.cspdL
				ptR = 1.e0 + self.G7*(um - self.velxR)/self.cspdR
				pm  = 0.5e0*(self.presL*pow(ptL,self.G3) \
					         + self.presR*pow(ptR,self.G3))
			else:
				# Use Two-Shock Riemann solver with PVRS as estimate
				geL = sqrt((self.G5/self.densL)/(self.G6*self.presL + ppv))
				geR = sqrt((self.G5/self.densR)/(self.G6*self.presR + ppv))
				pm  = (geL*self.presL + geR*self.presR \
					   - self.velxR + self.velxL)/(geL + geR)

		# 3) Return the pressure estimate in the star region
		return pm

	def prefun(self, P, densK, presK, cspdK):
		from math import pow, sqrt

		# Outputs
		F  = -1e99
		dF = -1e99

		# Rarefaction wave
		if(P <= presK):
			prat = P/presK
			F    = self.G4*cspdK*(pow(prat, self.G1) - 1.e0)
			dF   = (1.e0/(densK*cspdK))*pow(prat, -self.G2)

		# Shock wave
		else:
			aK  = self.G5/densK
			bK  = self.G6*presK
			qrt = sqrt(aK/(bK + P))
			F   = (P - presK)*qrt
			dF  = (1.e0 - 0.5e0*(P - presK)/(bK + P))*qrt

		return F, dF

	def samplePt(self, S):
		from math import pow, sqrt

		# Output
		dOut = -1e99
		uOut = -1e99
		pOut = -1e99

		# Left of contact discontinuity
		if(S <= self.velxS):
			# Left rarefaction
			if(self.presS <= self.presL):
				shl = self.velxL - self.cspdL
				# Left initial state
				if(S <= shl):
					dOut = self.densL
					pOut = self.presL
					uOut = self.velxL
				else:
					cml = self.cspdL*pow(self.presS/self.presL,self.G1)
					stl = self.velxS - cml
					# Left star state
					if(S > stl):
						dOut = self.densL*pow(self.presS/self.presL,1.0/self.gamma)
						uOut = self.velxS
						pOut = self.presS
					# Left fan
					else:
						uOut = self.G5*(self.cspdL + self.G7*self.velxL + S)
						c    = self.G5*(self.cspdL + self.G7*(self.velxL - S))
						dOut = self.densL*pow(c/self.cspdL,self.G4)
						pOut = self.presL*pow(c/self.cspdL,self.G3)
			# Left shock
			else:
				pml = self.presS/self.presL
				sl  = self.velxL - self.cspdL*sqrt(self.G2*pml + self.G1)
				# Left initial state
				if(S <= sl):
					dOut = self.densL
					pOut = self.presL
					uOut = self.velxL
				# Left star state
				else:
					dOut = self.densL*(pml + self.G6)/(self.G6*pml + 1.e0)
					pOut = self.presS
					uOut = self.velxS
		# Right of contact discontinuity
		else:
			# Right shock
			if(self.presS > self.presR):
				pmr = self.presS/self.presR
				sr  = self.velxR + self.cspdR*sqrt(self.G2*pmr + self.G1)
				# Right initial state
				if(S >= sr):
					dOut = self.densR
					pOut = self.presR
					uOut = self.velxR
				# Right star state
				else:
					dOut = self.densR*(pmr + self.G6)/(self.G6*pmr + 1.e0)
					pOut = self.presS
					uOut = self.velxS
			# Right rarefaction
			else:
				shr = self.velxR + self.cspdR
				# Right initial state
				if(S >= shr):
					dOut = self.densR
					pOut = self.presR
					uOut = self.velxR
				else:
					cmr = self.cspdR*pow(self.presS/self.presR,self.G1)
					stR = self.velxS + cmr
					# Right star state
					if(S <= stR):
						dOut = self.densR*pow(self.presS/self.presR,1.e0/self.gamma)
						pOut = self.presS
						uOut = self.velxS
					# Right fan
					else:
						uOut = self.G5*(-self.cspdR + self.G7*self.velxR + S)
						c    = self.G5*(self.cspdR - self.G7*(self.velxR - S))
						dOut = self.densR*pow(c/self.cspdR,self.G4)
						pOut = self.presR*pow(c/self.cspdR,self.G3)
		# Done!
		return dOut, pOut, uOut

	def sample(self,sPts):
		from math import sqrt
		dens = []
		pres = []
		velx = []
		eint = []
		cspd = []

		for S in sPts:
			d, p, u = self.samplePt(float(S))
			dens.append(d)
			pres.append(p)
			velx.append(u)
			eint.append((p/d)/(self.gamma-1.e0))
			cspd.append(sqrt(self.gamma*p/d))

		return dens, pres, velx, eint, cspd
