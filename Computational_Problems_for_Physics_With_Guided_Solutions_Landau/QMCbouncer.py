""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# QMCbouncer.py:       g.s. wavefunction via path integration

from visual import *
import random
from visual.graph import *

# Parameters
N = 100;  dt = 0.05;          g = 2.0;       h = 0.00;      maxel = 0
path = zeros([101], float);  arr = path; prob = zeros([201],float)                   
    
trajec = display(width = 300, height=500,title = 'Spacetime Trajectory')
trplot = curve(y = range(0, 100), color=color.magenta, display = trajec)

def trjaxs():                               # plot axis for trajectories
  trax=curve(pos=[(-97,-100),(100,-100)],color=color.cyan,display=trajec)
  curve(pos = [(-65, -100),(-65, 100)], color=color.cyan,display=trajec)
  label(pos = (-65,110), text = 't', box = 0, display = trajec)
  label(pos = (-85, -110), text = '0', box = 0, display = trajec)
  label(pos = (60, -110), text = 'x', box = 0, display = trajec) 
wvgraph = display(x=350, y=80, width=500, height=300, title = 'GS Prob')
wvplot  = curve(x = range(0, 50), display = wvgraph)  # wave function plot
wvfax   = curve(color = color.cyan)

def wvfaxs():                                # plot axis for wavefunction
   wvfax = curve(pos =[(-200,-155),(800,-155)],display=wvgraph,color=color.cyan)
   curve(pos = [(-200,-150),(-200,400)],display=wvgraph,color=color.cyan)
   label(pos = (-70, 420),text = 'Probability', box = 0, display=wvgraph)
   label(pos = (600, -220),text = 'x', box = 0, display = wvgraph)
   label(pos = (-200, -220),text = '0', box = 0, display = wvgraph)
                       
trjaxs();  wvfaxs()                                           # plot axes

def energy (arr):                                        # Energy of path
    esum = 0.                          
    for i in range(0,N):
      esum += 0.5*((arr[i+1]-arr[i])/dt)**2+g*(arr[i]+arr[i+1])/2                                                         
    return esum

def plotpath(path):                                  # Plot xy trajectory
   for j in range (0, N):                     
       trplot.x[j] = 20*path[j] - 65
       trplot.y[j] = 2*j - 100

def plotwvf(prob):                                    # Plot wave function
    for i in range (0, 50):
       wvplot.color = color.yellow
       wvplot.x[i] = 20*i - 200
       wvplot.y[i] = 0.5*prob[i] - 150
     
oldE = energy(path)                                            
counter = 1                                       
norm = 0.                                            # plot psi every 100 
maxx = 0.0

while 1:                                                # "Infinite" loop
    rate(100)
    element = int(N*random.random() )
    if element != 0 and element!= N:                   # Ends not allowed
        change = ( (random.random() - 0.5)*20.)/10.   
        if  path[element] + change > 0.:              # No negative paths
          path[element]   += change                   
          newE = energy(path)                          # New trajectory E
          if newE > oldE and exp( - newE + oldE) <= random.random() :   
             path[element]   -= change                    # Link rejected
             plotpath(path)                        
          ele = int(path[element]*1250./100.)             # Scale changed
          if  ele >= maxel:  maxel = ele            # Scale change 0 to N                                           
          if  element != 0:  prob[ele]   += 1 
          oldE = newE;                             
    if counter%100 == 0:                             # plot psi every 100
        for  i in range(0, N):                            # max x of path
            if  path[i] >= maxx:  maxx = path[i]           
        h = maxx/maxel                                       # space step
        firstlast = h*0.5*(prob[0] +  prob[maxel])   # for trap. extremes
        for  i in range(0, maxel + 1):  norm = norm + prob[i]      # norm
        norm = norm*h + firstlast                             # Trap rule
        plotwvf(prob)                                 # plot probability
    counter   += 1
