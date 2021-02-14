""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
		
# QMC.py: Quantum MonteCarlo (Feynman path integration)

from visual import *;  from visual.graph import *;  import random

N = 100; Nsteps = 101;  xscale = 10.                 # Initialize
path = zeros([Nsteps], float);  prob = zeros([Nsteps], float)   

trajec = display(width = 300,height=500, title='Spacetime Paths')
trplot = curve(y = range(0, 100), color=color.magenta, display = trajec)

def PlotAxes():                                       # Axis
   trax = curve(pos=[(-97,-100),(100,-100)],colo =color.cyan,display=trajec)
   label(pos = (0,-110),  text = '0', box = 0, display = trajec)
   label(pos = (60,-110), text = 'x', box = 0, display = trajec) 
def WaveFunctionAxes():                     # Axes for probability
   wvfax=curve(pos =[(-600,-155),(800,-155)],display=wvgraph,color=color.cyan)
   curve(pos = [(0,-150), (0,400)], display=wvgraph, color=color.cyan) 
   label(pos = (-80,450), text='Probability', box = 0, display = wvgraph)
   label(pos = (600,-220), text='x', box=0, display=wvgraph)
   label(pos = (0,-220), text='0', box=0, display=wvgraph)   
def Energy(path):                                   # HO Energy
    sums = 0.                                
    for i in range(0,N-2):sums += (path[i+1]-path[i])*(path[i+1]-path[i])
    sums += path[i+1]*path[i+1]; 
    return sums 
def PlotPath(path):                               # Plot trajectory
   for j in range (0, N):                     
       trplot.x[j] = 20*path[j]
       trplot.y[j] = 2*j - 100   
def PlotWF(prob):                                      # Plot prob
    for i in range (0, 100):
       wvplot.color = color.yellow
       wvplot.x[i] = 8*i - 400                         # Center fig
                   
wvgraph = display(x=340,y=150,width=500,height=300,
	title='Ground State')
wvplot = curve(x = range(0, 100), display = wvgraph)                
wvfax = curve(color = color.cyan)
PlotAxes();  WaveFunctionAxes()                       # Plot axes
oldE = Energy(path)                                      
while True:                                 # Pick random element
    rate(10)                                     # Slow paintings
    element = int(N*random.random() )               # Metropolis
    change = 2.0*(random.random() - 0.5)    
    path[element] += change                        # Change path
    newE = Energy(path);                             # Find new E
    if  newE > oldE and math.exp( - newE + oldE)<= random.random():
          path[element] -= change                     # Reject
          PlotPath(path)                     # Plot trajectory
    elem = int(path[element]*16 + 50)    # if path = 0, elem = 50
    
# elem = m *path[element] + b is the linear transformation
# if path=-3, elem=2 if path=3., elem=98 => b=50, m=16 linear TF
# this way x = 0 correspond to prob[50]
    
    if elem < 0: elem = 0,                     
    if elem > 100:  elem = 100                 # If exceed max
    prob[elem] += 1                     # increase probability
    PlotWF(prob)                       w            # Plot prob
    oldE = newE                
