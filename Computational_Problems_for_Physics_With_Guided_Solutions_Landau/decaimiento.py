""" From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation """

#Simulates radiactive decay
from visual import *
from visual.graph import *
import random                      # generates random numbers

rejilla = ones((100,100))       # grid 100x100
escena = display(x=0,y=0, width=400, height=400, range=120,
                  title='Simulates radiactive decay') #to see atoms
graph=gdisplay(x=0,y=400,width=600, height=400, title='Nuclei left',
               xtitle='time (s)',ytitle='N(t)',xmax=50000,
               xmin=0.0,ymax=2000,ymin=0,foreground=color.black,
               background=color.white) # to see decay cruve
restantes=gcurve(color=color.red)  #curve in red, instance of gcurve
Natomos=0                          # At beginning  0 atoms
def atomos():                      # put atoms at random in grid
    global Natomos                 # to know the number
    for i in range (0,2000):
        x=int(100*random.random())  #coord. x integer 0<=x<=100
        y=int(100*random.random())  #coord. y integer 0<=y<=100
        if rejilla[x][y]==1:        #empty this position?
            xpos=2*x-100            # x screen position
            ypos=2*y-100            # y screen position
            sphere(pos=(xpos,ypos),color=color.green,radius=2.0)#atom
            rejilla[x][y]=0         # occupied cell
            Natomos+=1              #another atom placed
    print Natomos                   #numero de atomos colocados
def decaimiento():                  #calcula y grafica el decaimiento
    global Natomos                  #use el valor que calculo de Natomos
    atomos()                        #la llama para colocar los atomos
    constdec=0.8                    #constante de decaimiento
    print Natomos
   
    for t in range(0,50000):        # 50000 events
        rate(3000)                  #slow action
        x=int(100*random.random())  #generates coord. x  0 to 100
        y=int(100*random.random())  #generates y coord. between 0 - 100
        r=random.random()           #random number between 0 and 1
        #following: if atom at xy, if r< decay constant
        # and if atoms to decay
        if rejilla[x][y]==0 and r<constdec and Natomos>0: 
            Natomos-=1              # atom decay
            xpos=2*x-100            #coord x in screen
            ypos=2*y-100            #coord y in screen
            sphere(pos=(xpos,ypos),color=color.white,radius=2.0) 
            rejilla[x][y]=2         #indicates atom at that point decayed
            restantes.plot(pos=(t,Natomos)) #plot nondecayed atoms
                            
decaimiento()      
