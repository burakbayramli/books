""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# MDinBox.py: MD simulation in a box

from visual.graph import *
import random

L=1                                    # side square
scene = display(width=500,height=500,range=(1.3)  )
ndist = gdisplay(x=500, ymax = 200,
             width=500, height=500, xtitle='Nr', ytitle='N')
inside=label(pos=(0.4,1.1),text='Particles here=',box=0)
inside2=label(pos=(0.8,1.1),box=0)
Natom=16                                # number of atoms
Nr=0                                # number particles right side
dt=1e-6                             # time step
border=curve(pos=[(-L,-L),(L,-L),(L,L),(-L,L),(-L,-L)]) #limits figure
half=curve(pos=[(0,-L),(0,L)],color=color.yellow)       # middle
positions=[]                           # position of atoms
vel=[]                                 # vel of atoms
Atom=[]                                # will contain spheres
dN=[]         # will contain atoms in right half at each tieme interval
fr=[0]*(Natom)                         # atoms (spheres)
fr2=[0]*(Natom)                        # second  force
ar=0.03                                # radius of atom
pref=4                                 # a reference velocity
h=0.01
factor=1e-9                           # for lennard jones
deltaN=1                               # for histogram
distribution = ghistogram(bins=arange(0.,Natom,deltaN),
                        accumulate=1, average=1, color=color.red)
for i in range (0,Natom):              # initial positions and velocities
    col=(1.3*random.random(),1.3*random.random(),1.3*random.random())
    x=2.*(L-ar)*random.random()-L+ar   #positons of atoms
    y=2.*(L-ar)*random.random()-L+ar   # in the border forbidden
    Atom=Atom+[sphere(pos=(x,y),radius=ar,color=col)] # add atoms
    theta=2*pi*random.random()         # select angle  0<=theta<= 2pi
    vx=pref*cos(theta)                 # x component velocity
    vy=pref*sin(theta)
    positions.append((x,y))            # add positions to list
    vel.append((vx,vy))                # add momentum to list
    pos=array(positions)               # array with positions
    ddp=pos[i]
    if ddp[0]>=0 and ddp[0]<=L:        # count initial atoms at right half
           Nr+=1    
    v=array(vel)                       # array of velocities
#print('Nr',Nr)    
def sign(a, b):                        # sign function
    if (b >=  0.0):
        return abs(a)
    else:
        return  - abs(a)
def forces(fr):          
     fr=[0]*(Natom)
     for i in range( 0, Natom-1 ):
          for j in range(i + 1, Natom):
              dr=pos[i]-pos[j]          # relative position between particles
              
              if (abs(dr[0]) > L):       # smallest distance from part.or image
                  dr[0] = dr[0]  -  sign(2*L, dr[0])  # interact with closer image
              if (abs(dr[1]) > L):       # same for y
                  dr[1] = dr[1]  -  sign(2*L, dr[1])
              if i==0 and j==1:
                  curve(pos=[(pos[0]),(pos[0]-dr)])    
              r2=mag2(dr)
              if (abs(r2) < ar):       # to avoid 0 denominator
                  r2 = ar              # 
              invr2 = 1./r2              # compute this factor
              fij =invr2*factor*  48.*(invr2**3 - 0.5) *invr2**3 #
              fr[i]=fij*dr+ fr[i]
              fr[j]=-fij*dr +fr[j]       # lennard jones force
     return fr       
        

for t in range (0,1000):                 # time steps
     Nr=0                                 # begin at zero in each time
     for i in range(0,Natom):
        fr=forces(fr)
        dpos=pos[i]
        if dpos[0] <= -L:
                pos[i] = [dpos[0]+2*L,dpos[1]] # x periodic boundary conditions
        if dpos[0] >= L:
                pos[i] = [dpos[0]-2*L,dpos[1]]
        if dpos[1] <= -L:
                pos[i] = [dpos[0],dpos[1]+2*L] # y periodic boundary conditions
        if dpos[1] >= L:
                pos[i] = [dpos[0],dpos[1]-2*L]
        dpos=pos[i]
        if dpos[0]>0 and dpos[0]<L:            # count particles at right
            Nr+=1
        fr2=forces(fr)
        fr2=fr
        v[i]=v[i]+0.5*h*h*(fr[i]+fr2[i])       # velocity Verlet algorithm
        pos[i]=pos[i]+h*v[i]+0.5*h*h*fr[i]
        Atom[i].pos=pos[i]                     # plot atoms at new positions     
     #print(Nr)
     inside2.text='%4s'%Nr                      # particles in right side
     dN.append(Nr)                             # for the histogram
     distribution.plot(data=dN)                # plot histogram 


     
       
     
          
    
        
   

