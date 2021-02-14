""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
	
# Gameoflife.py:       Cellular automata in 2 dimensions

'''* Rules: a cell can be either dead (0) or alive (1)
   * If a cell is alive:
   * on next step will remain alive if
   * 2 or 3 of its closer 8 neighbors are alive.
   * If > 3 of 8 neighbors are alive, cell dies of overcrowdedness
   * If less than  2 neighbors are alive the cell dies of loneliness
   * A dead cell will be alive if  3 of its 8 neighbors are alive'''

from visual import *
from visual.graph import * ; import random

scene = display(width= 500,height= 500, title= 'Game of Life') 
cell  = zeros((50,50));        cellu = zeros((50,50))
curve(pos= [(-49,-49),(-49,49),(49,49),(49,-49),(-49,-49)],color=color.white)
boxes = points(shape='square', size=8, color=color.cyan)

def drawcells(ce):
    boxes.pos = []                                 # Erase previous cells
    for j in range(0,50):       
        for i in range(0,50):
            if ce[i,j] == 1:
                xx = 2*i-50
                yy = 2*j-50
                boxes.append(pos=(xx,yy))
            
def initial():
   for j in range (20,28):                        
        for  i in range(20, 28):  
            r= int(random.random()*2)
            cell[j,i] = r                        
   return cell           

def gameoflife(cell):                   
    for i in range(1,49):               
        for j in range(1,49):
            sum1 = cell[i-1,j-1] + cell[i,j-1] + cell[i+1,j-1] # neighb
            sum2 = cell[i-1,j] + cell[i+1,j] + cell[i-1,j+1] \
               + cell[i,j+1] + cell[i+1,j+1] 
            alive = sum1+sum2
            if cell[i,j] == 1:
                if  alive == 2 or alive == 3:                     # Alive
                    cellu[i,j] = 1                                # Lives
                if  alive > 3 or alive < 2:     # Overcrowded or solitude
                    cellu[i,j] = 0                                 # dies
            if  cell[i,j] == 0:
                if  alive == 3:
                    cellu[i,j] = 1                              # Revives
                else:
                    cellu[i,j] = 0                         # Remains dead
    alive = 0                
    return cellu                
temp = initial()               
drawcells(temp)
while True:
    rate(6)
    cell = temp
    temp = gameoflife(cell)
    drawcells(cell) 
