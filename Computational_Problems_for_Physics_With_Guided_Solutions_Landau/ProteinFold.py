""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# ProteinFold.py: Self avoiding random walk
# Stops in corners or  occupied neighbors
# energy  =  -f|eps, f=1 if neighbour = H, f=0 if p
# Yellow dot indicates unconnected neighbor

from visual import *;  import random

Maxx = 500;  Maxy = 500;  ran = 20; L = 100;  m = 100
size  = 8;  size2 = size*2;  nex = 0;  n = 100
M = []; DD = []              # Arrays for polymer & grid

graph1 = display(width=Maxx, height=Maxy,
	title='Protein Folding', range=ran)
positions = points(color=color.cyan,size = 2)

def selectcol():                    # Select atom's colors
    hp = random.random()                  # Select H or P   
    if hp <= 0.7:            
        col = (1,0,0)             # Hydrophobic color red
        r = 2               
    else:
        col = (1,1,1)                 # Polar color white
        r = 1               
    return col,r

def findrest(m,length,fin,fjn):     # Check links energies
    ener = 0
    for t in range(m,length+1):  # Next link not considered 
        if DD[t][0]==fin and DD[t][1]==fjn and DD[t][2]==2:
            ener = 1               # Red unlinked neighbor
    return ener
        
def findenergy(length,DD):       # Finds energy of each link
    energy = 0                      
    for n in range (0,length+1):
        i = DD[n][0]
        j = DD[n][1]
        cl = DD[n][2]       
        if cl==1: pass                           # if white
        else:                                         # red 
           if n < length+1:
              imin = int(i-1)        # Check neighbor i-1,j
              js = int(j)
              if imin >= 0:
                  e = findrest(n+2,length,imin,js) 
                  energy = energy + e
                  if e==1:        # Yellow dot at neighbor
                       xol = 4*(i-0.5)-size2
                       yol = -4*j+size2
                       points(pos=(xol,yol),color=color.yellow, 
                       	       size=6)
              ima = i+1
              js = j
              if ima<=size-1:      # Check neighborr i+1,j
                  e = findrest(n+2,length,ima,js)
                  energy = energy+e
                  if e == 1:     # Yellow dot at neighbor
                       xol = 4*(i+0.5)-size2  
                       yol = -4*j+size2     
                       points(pos=(xol,yol),color=color.yellow, 
                       	       size=6)
              iss = i
              jma = j+1
              if jma <= size-1:     # Check neighbor i,j+1
                  e = findrest(n+2,length,iss,jma)
                  energy = energy+e       
                  if e == 1:        # Yellow dot at neighbor
                       xol = 4*i-size2       # Start at middle
                       yol = -4*(j+0.5)+size2
                       points(pos=(xol,yol),color=color.yellow, 
                       	       size=6)
              iss = i
              jmi = j-1
              if jmi >= 0:        # Check neighbor i, j-1
                 e = findrest(n+2,length,iss,jmi)
                 energy = energy +e
                 if e==1:         # Yellow dot at neighbor
                       xol = 4*i-size2    # Start at middle
                       yol = -4*(j-0.5)+size2   
                       points(pos=(xol,yol),color=color.yellow,
                       	       size=6)     
    return energy   
    
def grid():                                        # Plot grid    
    for j in range(0,size):  
        yp = -4*j+size2                  # World to screen coord
        for i in range (0,size):            # Horizontal row
            xp = 4*i-size2
            positions.append(pos = (xp,yp))
grid()
length = 0
while 1:                             
    pts2 = label(pos=(-5, -18), box=0)   
    length = 0                           
    grid = zeros((size,size))          
    D = zeros((L,m,n))
    DD = []
    i = size/2                          # Center of grid 
    j = size/2
    xol = 4*i-size2                
    yol = -4*j+size2                
    col,c = selectcol()
    grid[i,j] = c                    # Particle in center
    M = M+[points(pos=(xol,yol),color=col, size=6)] # Red center
    print(" start     ")
    DD = DD+[[i,j,c]]   
    while (i>0 and i<size-1 and j>0 and j<size-1 
    	    and (grid[i+1,j] == 0
            or grid[i-1,j] == 0 or grid[i,j+1] == 0 
            or grid[i,j-1] == 0)):
        r = random.random()       
        if r < 0.25 :                   # Probability 25%
            if grid[i+1,j]==0:  i += 1  # Step R if empty      
        elif 0.25 < r and r < 0.5:     # Step L
            if grid[i-1,j] == 0: i -= 1    
        elif 0.50 < r and r < 0.75:        # Up
            if grid[i,j-1]==0:  j -= 1
        else :                             # Down
            if grid[i,j+1]==0:  j+=1
        if grid[i,j] == 0:
           col,c = selectcol()
           grid[i,j] = 2                 # Occupy grid point
           length += 1         # Increase length as occupied
           DD = DD+[[i,j,c]]
           xp = 4*i-size2       
           yp = -4*j+size2                   
           curve(pos=[(xol,yol),(xp,yp)])# Connect last to new
           M = M + [points(pos=(xp,yp), color=col,size=6)]
           xol = xp                           # Start new line
           yol = yp                          
        while (j == (size-1) and i != 0 and i != (size-1)):
            r1 = random.random()
            if r1 < 0.2:                    # Prob 20% move left
                if grid[i-1,j] == 0: i -= 1
            elif r1 > 0.2 and r1 < 0.4:     # Prob 20% move right
                if grid[i+1,j] == 0: i += 1
            else:                           # Prob 60% move up
                 if grid[i,j-1] == 0:  j-=1
            if grid[i,j] == 0:
               col,c = selectcol()          # Increase length
               grid[i,j] = 2                # Grid point occupied 
               length += 1
               DD = DD + [[i,j,c]]
               xp = 4*i - size2                          
               yp = -4*j + size2
               curve(pos=[(xol,yol),(xp,yp)])   
               M = M +[points(pos=(xp,yp), color=col,size=6)]
               xol = xp                            
               yol = yp    # Last row; Stop if corner or neighbors
            if (i==0 or i==(size-1)) or (grid[i-1,size-1]!=0
            	    and grid[i+1,size-1]!=0):
                break
        while (j == 0 and i != 0 and i != (size-1)): # First row
            r1 = random.random()
            if r1<0.2:
                if grid[i-1,j] == 0:  i -= 1
            elif r1>0.2 and r1<0.4:
                if grid[i+1,j]==0:    i += 1
            else:
                if grid[i,j+1]==0:    j += 1
            if grid[i,j]==0:
               col,c = selectcol()
               grid[i,j] = 2
               length += 1
               DD = DD + [[i,j,c]]
               xp = 4*i - size2
               yp = -4*j + size2
               curve(pos=[(xol,yol),(xp,yp)])
               M = M + [points(pos=(xp,yp), color=col,size=6)]
               xol = xp
               yol = yp
            if i==(size-1) or i==0 or (grid[i-1,0]!=0 
            	    and grid[i+1,0]!=0):
                break
        while (i==0 and j !=0 and j !=(size-1)): # First column
            r1 = random.random()
            if r1<0.2:
                if grid[i,j-1] == 0:  j -= 1
            elif r1 > 0.2 and r1 < 0.4:
                if grid[i,j+1] == 0:  j += 1
            else:
                if grid[i+1,j] == 0:  i += 1
            if grid[i,j] == 0:
               col,c = selectcol()
               grid[i,j] = c
               length += 1
               DD = DD+[[i,j,c]]
               xp = 4*i - size2
               yp = -4*j + size2
               curve(pos=[(xol,yol),(xp,yp)])
               M = M +[points(pos=(xp,yp), color=col,size=6)]
               xol = xp
               yol = yp
            if j==(size-1) or j==0 or (grid[0,j+1]!=0 
            	    and grid[0,j-1]!=0):
                break
        while (i==(size-1) and j !=0 and j !=(size-1)): # Last col
            r1 = random.random()
            if r1 < 0.2:
                if grid[i,j-1] == 0: j -= 1
            elif r1 > 0.2 and r1 < 0.4:
                if grid[i,j+1] == 0:  j += 1
            else:
                if grid[i-1,j] == 0:  i -= 1
            if grid[i,j] == 0:
               col,c = selectcol()
               grid[i,j] = c
               length += 1
               col,c=selectcol()
               DD = DD + [[i,j,c]]
               xp = 4*i - size2
               yp = -4*j + size2
               curve(pos=[(xol,yol),(xp,yp)])
               M = M +[points(pos=(xp,yp), color=col,size=6)]
               xol = xp
               yol = yp
            if j==(size-1) or (grid[size-1,j+1]!=0 
            	    and grid[size-1,j-1]!=0):
                break
    label(pos=(-10, -18), text='Length=', box=0)     
    label(pos=(10,18,0), text='Click for new walk',
    	    color=color.red, display=graph1)
    pts2.text = '%4s' %length       
    label(pos=(5,-18,0), text='Energy',box=0)
    evalue=label(pos=(10, -18), box=0) # Energy
    evalue.text = '%4s' %findenergy(length,DD)   
    print("energy is ",findenergy(length,DD))
    print("dd") 
    graph1.mouse.getclick()         # Detect mouse click
    for obj in graph1.objects:      # Start new walk
        if (obj is positions or obj is curve):     continue
        obj.visible = 0  # Clear curve
