""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# MDsplitBox.py: Counting particle in split box

from visual.graph import *
import random
                          
L=1;  deltaN = 1;  Natoms = 16;  dt = 1e-6;   ar = 0.03; t = 1
twoN = 2**Natoms;  Nrhs = 0   
prob = [0]*(Natoms); dN = [0]*(Natoms); Atom = []

scene = display(width=400,height=400,range=(1.3)  )
ndist = gdisplay(x=400, ymax = 100, width=400, height=400, xtitle='Nrhs',
         ytitle='N',title='Numb Times Nrhs Particles Occur')
distribution = ghistogram(bins=arange(1., Natoms, deltaN),
                       accumulate=1, average=1, color=color.red)
scene2 = gdisplay(y=400,height=400,width=400,xmin=0,xmax=16,ymin=0,ymax=0.01,
                title='Probability of Nrhs',
                ytitle='P(n)/2**N',xtitle='Nrhs')
dis = gvbars(delta=.5,color=color.red,display=scene2)

while t < 1000:
   positions = []                            
   curve(pos = [(-L,-L),(L,-L),(L,L),(-L,L),(-L,-L)])  
   curve(pos=[(0,-L),(0,L)],color=color.yellow)
   inside = label(pos=(0.2,1.1),text='Numb RHS Particles =',box=0)
   inside2 = label(pos=(0.8,1.1),box=0)
   for i in range (Natoms):              # Initial x & v
      rate(10)
      x = 2.*(L-ar)*random.random() - L + ar    # Atom locations
      y = 2.*(L-ar)*random.random() - L + ar    
      Atom = Atom+[sphere(pos=(x,y),radius=ar,color=color.green) ] 
      positions.append((x,y))            
      pos = array(positions)               
      ddp = pos[i]
      if ddp[0]>=0 and ddp[0]<=L: Nrhs += 1     # Count initial           
      inside2.text = '%4s'%Nrhs
   dN.append(Nrhs)                                  # Histogram
   distribution.plot(data=dN)                  # Plot histogram
   prob[Nrhs] += 1
   dis.plot(pos=(Nrhs,prob[Nrhs]/twoN))         # Probabilities
   for obj in scene.objects:                   # Start new walk
        if (obj is curve or obj is sphere or obj is label) : continue
        else: obj.visible = 0                        # Clear curve
   inside2.text = '%4s'%Nrhs     
   Nrhs = 0
   t += 1
