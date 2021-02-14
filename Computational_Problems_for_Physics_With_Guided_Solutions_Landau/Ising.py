""" From "A SURVEY OF COMPUTATIONAL PHYSICS", Python eBook Version
   by RH Landau, MJ Paez, and CC Bordeianu
   Copyright Princeton University Press, Princeton, 2011; Book  Copyright R Landau, 
   Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2011.
   Support by National Science Foundation , Oregon State Univ, Microsoft Corp"""  

# Ising.py: Ising model
from visual import *
import random
from visual.graph import *

# Display for the arrows
scene = display(x=0,y=0,width=700,height=200, range=40,title='Spins')
engraph = gdisplay(y=200,width=700,height=300, title='E of Spin System',\
        xtitle='iteration', ytitle='E',xmax=500, xmin=0, ymax=5, ymin=-5)
enplot = gcurve(color=color.yellow)                 # for the energy plot
N     = 30                                              # number of spins
B     = 1.                                               # magnetic field
mu    = .33                        # g mu (giromag. times Bohrs magneton)
J     = .20                                             # Exchange energy
k     = 1.                                            # Boltmann constant
T     = 100.                                                # Temperature
state = zeros((N))                 # spins state some up(1) some down (0)
S     = zeros((N) ,float)                   
test  = state                                              # a test state
random.seed()                                     # Seed random generator

def energy ( S) :                                 # Method to calc energy
    FirstTerm = 0.
    SecondTerm = 0.                                         # Sum  energy
    for  i in range(0,N-2):  FirstTerm += S[i]*S[i + 1]
    FirstTerm *= -J 
    for i in range(0,N-1):   SecondTerm += S[i]
    SecondTerm *= -B*mu; 
    return (FirstTerm + SecondTerm); 
		
ES = energy(state)                                 # State, test's energy

def spstate(state):                      # Plots spins according to state
    for obj in scene.objects: obj.visible=0      #  erase previous arrows
    j=0    
    for i in range(-N,N,2):              # 30 spins numbered from 0 to 29
        if state[j]==-1:  ypos = 5                       # case spin down
        else:             ypos = 0
        if  5*state[j]<0: arrowcol = (1,1,1)   # white arrow if spin down
        else:             arrowcol =(0.7,0.8,0)
        arrow(pos=(i,ypos,0),axis=(0,5*state[j],0),color=arrowcol)# arrow
        j +=1
        
for  i in range(0 ,N):  state[i] = -1     # initial state, all spins down

for obj in scene.objects:   obj.visible=0
spstate(state)                      # plots initial state: all spins down
ES = energy(state)                  # finds the energy of the spin system
                                    # Here is the Metropolis algorithm
for  j in range (1,500):            # Change state and test
      rate(3)                       # to be able to see the flipping
      test = state                  # test is the previous spin state
      r = int(N*random.random());   # Flip spin randomly
      test[r] *= -1                 # flips temporarily that spin
      ET = energy(test)             # finds energy of the test configur.
      p = math.exp((ES-ET)/(k*T))   # test with Boltzmann factor
      enplot.plot(pos=(j,ES))       # adds a segment to the curve of E 
      if p >= random.random():      # to see if trial config. is accepted
           state = test
           spstate(state)
           ES = ET
              
