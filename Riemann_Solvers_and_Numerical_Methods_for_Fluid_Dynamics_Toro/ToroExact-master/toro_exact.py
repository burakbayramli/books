#!/usr/bin/env python3

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

import sys
import ast
import exactRP
import argparse

#============================================
# Problem definitions for Toro (1999) problems
#============================================
cases = {
	'1': {'left':[1.0,      0.75,       1.0],   'right':[0.125,     0.0,      0.1],    'time':0.2,   'x0':0.3},
	'2': {'left':[1.0,     -2.0,        0.4],   'right':[1.0,       2.0,      0.4],    'time':0.15,  'x0':0.5},
	'3': {'left':[1.0,      0.0,     1000.0],   'right':[1.0,       0.0,      0.01],   'time':0.012, 'x0':0.5},
	'4': {'left':[5.99924, 19.5975,   460.894], 'right':[5.99242,  -6.19633, 46.0950], 'time':0.035, 'x0':0.4},
	'5': {'left':[1.0,    -19.59745, 1000.0],   'right':[1.0,     -19.59745,  0.01],   'time':0.012, 'x0':0.8},
	'6': {'left':[1.4,      0.0,        1.0],   'right':[1.0,       0.0,      1.0],    'time':2.0,   'x0':0.5},
	'7': {'left':[1.4,      0.1,        1.0],   'right':[1.0,       0.1,      1.0],    'time':2.0,   'x0':0.5},
}

#============================================
# setupParser: Setup command line argument parser
#============================================
def setupParser():
	'''Setup the argument parser and return the parser object'''

	parser = argparse.ArgumentParser()
	parser.add_argument('-p','--problem',dest='problems',
		                action='append', required=True,
		                choices=['1','2','3','4','5','6','7','all','user'],
		                help='select which problem to solve')
	parser.add_argument('-g','--gamma',dest='gamma',
		                default=1.4,
		                help='set ideal gas gamma')
	parser.add_argument('-d','--disc',dest='disc',
		                default='zonal', choices=['zonal','nodal'],
		                help='set discretization [zonal, nodal]')
	parser.add_argument('-n','--npts',dest='npts',
		                default=100,
		                help='set number of evaluation points')
	parser.add_argument('-b','--bounds',dest='bounds',
		                default='[0.0,1.0]',
		                help='set domain boundaries')

	user_group = parser.add_argument_group()
	user_group.add_argument('-l','--left',dest='stateL',
		                    help='set user left state "[dens, vel, pres]"')
	user_group.add_argument('-r','--right',dest='stateR',
		                    help='set user right state "[dens, vel, pres]"')
	user_group.add_argument('-x','--x0',dest='x0',
		                    help='set user diapgram location')
	user_group.add_argument('-t','--time',dest='time',
		                    help='set user evaluation time')
	return parser

#============================================
#                   MAIN
#============================================
if(__name__ == '__main__'):

	#--------------------------------
	# Handle arguments
	#--------------------------------
	parser = setupParser()
	args = parser.parse_args()

	# Extract parameters relevant to all problems
	problemList = args.problems
	if('all' in problemList):
		tmp = ['1','2','3','4','5','6','7']
		if('user' in problemList):
			tmp.append('user')
		problemList = tmp

	gamma = float(args.gamma)
	disc  = args.disc
	npts  = int(args.npts)
	xbnds = ast.literal_eval(args.bounds)

	# Extract user parameters
	if('user' in problemList):
		if(args.stateL is None or args.stateR is None
		   or args.x0 is None or args.time is None):
			parser.error('User problems require state{L/R}, x0 and time')

		cases['user'] = {'left':ast.literal_eval(args.stateL),
		                 'right':ast.literal_eval(args.stateR),
		                 'time':ast.literal_eval(args.time),
		                 'x0':ast.literal_eval(args.x0)}

	# Evaluate all requested problems
	for icase in problemList:
		stateL = cases[icase]['left']
		stateR = cases[icase]['right']
		t      = cases[icase]['time']
		x0     = cases[icase]['x0']

		sys.stdout.write('Running problem: {:s}\n'.format(icase))
		sys.stdout.write('  xbnds: {:f} {:f}\n'.format(xbnds[0],xbnds[1]))
		sys.stdout.write('  x0: {:f}\n'.format(x0))
		sys.stdout.write('  grid:  {:s} ({:d})\n'.format(disc,npts))
		sys.stdout.write('  left:  {:f}  {:f}  {:f}\n'.format(stateL[0], stateL[1], stateL[2]))
		sys.stdout.write('  right: {:f}  {:f}  {:f}\n'.format(stateR[0], stateR[1], stateR[2]))
		sys.stdout.write('  gamma: {:f}\n'.format(gamma))
		sys.stdout.write('  time:  {:f}\n'.format(t))

		# Solve the exact RP, which produces
		# the state in the star region
		rp = exactRP.exactRP(gamma, stateL, stateR)
		success = rp.solve()
		if(not success):
			sys.stdout.write('[FAILURE] Unable to solve problem {:s}'.format(icase))
			continue

		# Construct spatial and "speed" grid (x/t)
		x  = []
		s  = []
		shift = 0.0
		if(disc is 'zonal'):
			dx = (xbnds[1]-xbnds[0])/npts
			shift = 0.5e0
		else:
			dx = (xbnds[1]-xbnds[0])/(npts-1)
		for i in range(0,npts):
			x.append(xbnds[0] + (float(i)+shift)*dx)
			if(abs(t) <= 1.0e-9):
				s.append(x[i]-x0)
			else:
				s.append((x[i]-x0)/t)

		# Extract solution data on the requested grid
		dens, pres, velx, eint, cspd = rp.sample(s)

		# Write to output
		filename = 'output/toro_{:s}_exact.dat'.format(icase)
		sys.stdout.write('  Writing to file: {:s}\n'.format(filename))
		with open(filename,'w+') as file:
			file.write('# Exact Riemann solution for problem: {:s}\n'.format(icase))
			file.write('# Discretization: {:s}\n'.format(disc))
			file.write('# Domain bounds: [{:<.5e},{:.5e}]\n'.format(xbnds[0],xbnds[1]))
			file.write('# Iterface position: {:.5e}\n'.format(x0))
			file.write('# Gamma: {:.5e}\n'.format(gamma))
			file.write('# Left  state [dens,velx,pres]: [{:<.5e}, {:.5e}, {:.5e}]\n'.format(stateL[0],stateL[1],stateL[2]))
			file.write('# Right state [dens,velx,pres]: [{:<.5e}, {:.5e}, {:.5e}]\n'.format(stateR[0],stateR[1],stateR[2]))
			file.write('# Time: {:.5e}\n'.format(t))
			file.write('{:>17} {:>17} {:>17} {:>17} {:>17} {:>17}\n'.format('x','dens','pres','velx','eint','cspd'))
			for i in range(0,npts):
				file.write('{:17.9e} {:17.9e} {:17.9e} {:17.9e} {:17.9e} {:17.9e}\n'.format(x[i],dens[i],pres[i],velx[i],eint[i],cspd[i]))







