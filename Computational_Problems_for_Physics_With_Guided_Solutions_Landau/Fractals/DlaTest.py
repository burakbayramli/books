""" From  "COMPUTER PROBLEMS in PHYSICS"  by RH Landau & MJ Paez
    Copyright R Landau,   MJ Paez,    2017. 
    Please respect copyright & acknowledge our work."""



# DLA.py:   Diffusion Limited aggregation 

from visual.graph import *;  import random

Maxx = Maxy = 700;  R = 45.;  step =0;  trav = 0;  size = 80; max = 500                                   
grid = zeros((size,size))                   
grid[40,40] = 1                            # Particle in center
graph1 = display(width=Maxx, height=Maxy,title='Diffusion Limited Aggregation')
ring(pos=(0,0,0),axis=(0,0,1),radius=R,thickness=0.5,color=color.yellow)
sphere(pos=(0,0),radius=0.5,color=color.green)#central point
ball = sphere(radius=0.5)   

def gauss_ran():                          # Generate 2 Gaussian randoms
    old = 0                           
    r1 = 0                              
    rr = 0                              
    if (old == 0):                  
        while (rr >= 1 or rr == 0):     
            r1 = 2.*random.random() - 1.
            r2 = 2.*random.random() - 1.
            rr = r1*r1 + r2*r2
        fac = sqrt(-2.*log(rr)/rr)
        mem = int(15000.*r1*fac)
        old = 1
        return(int(15000.*r2*fac))
    else:
        old = 0
        return mem        

while True:               
    hit = 0                          
    angle = 2.*pi*random.random()  # Random spot on circle 
    x = 40 * int(R*cos(angle))     
    y = 40 * int(R*sin(angle))   
    dist = gauss_ran()                # Length of random walk
    trav = 0
    ballcolor = (0,1,0)
    while(hit==0 and x<79 and x>1 and y<79 and y>1 and trav < abs(dist)):
        if(random.random() <0.5):  step = 1                  
        else:  step = -1;
        if ((grid[x+1,y] + grid[x-1,y] + grid[x,y+1] + grid[x,y-1]) >= 1 ):
          hit = 1                          
          grid[x,y] = 1                    
          xc = x - 40                          
          yc = -y + 40                        
          sphere(pos=(xc,yc),radius=0.5,color=ballcolor)
        else:
            if ( random.random() < 0.5 ):  x += step
            else:  y += step                     
            xp = x - 40                          
            yp = -y + 40                        
            ball.color = ballcolor             
            ball.pos = (xp,yp,0)               
        trav = trav + 1     

