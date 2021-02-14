""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""
    
# Gameoflife.py: Cellular automata in 2 dimensions

'''* Rules: a cell can be either dead (0) or alive (1)
   * If a cell is alive:
   * on next step will remain alive if
   * 2 or 3 of its closer 8 neighbors are alive.
   * If > 3 of 8 neighbors are alive, cell dies of overcrowdedness
   * If less than  2 neighbors are alive the cell dies of loneliness
   * A dead cell will be alive if  3 of its 8 neighbors are alive'''

from visual import *
from visual.graph import * ; import random

scene = display(width= 500,height= 500, title= 'Game of Life')# 1st array
cell  = zeros((50,50));        cellu = zeros((50,50))
curve(pos= [(-49,-49),(-49,49),(49,49),(49,-49),(-49,-49)],color=color.white)
boxes = points(shape='square', size=8, color=color.cyan)

def drawcells(ce):
    boxes.pos = []                                 # erase previous cells
    for j in range(0,50):       
        for i in range(0,50):
            if ce[i,j] == 1:
                xx = 2*i-50
                yy = 2*j-50
                boxes.append(pos=(xx,yy))
            
def initial():
   for j in range (20,28):                       # initial state of cells
        for  i in range(20, 28):  
            r= int(random.random()*2)
            cell[j,i] = r                       # dead or alive at random
   return cell           

def gameoflife(cell):                  # rules and  analysis of neighbors
    for i in range(1,49):              # observe 8 neighbors of cell[i,j]
        for j in range(1,49):
            sum1 = cell[i-1,j-1] + cell[i,j-1] + cell[i+1,j-1] # sum neighb
            sum2 = cell[i-1,j] + cell[i+1,j] + cell[i-1,j+1] + cell[i,j+1] \
                   + cell[i+1,j+1] 
            alive = sum1+sum2
            if cell[i,j] == 1:
                if  alive == 2 or alive == 3:             # remains alive
                    cellu[i,j] = 1                                # lives
                if  alive > 3 or alive < 2:     # overcrowded or solitude
                    cellu[i,j] = 0                                 # dies
            if  cell[i,j] == 0:
                if  alive == 3:
                    cellu[i,j] = 1                              # revives
                else:
                    cellu[i,j] = 0                         # remains dead
    alive = 0                
    return cellu                
temp = initial()               
drawcells(temp)
while True:
    rate(6)
    cell = temp
    temp = gameoflife(cell)
    drawcells(cell) 
