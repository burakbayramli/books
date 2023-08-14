'''
Library for running FEM2D processes. 

Author: Daniel Coble
Copyright (c) 2022
MIT License
'''
import subprocess
import os
import numpy as np
from sklearn.preprocessing import PolynomialFeatures
from sklearn import linear_model
'''
FEM2DProblem contains all the variables used for
'''
class FEM2DProblemData():
    
    def set_default(self, var_name, default_val):
        if(not var_name in self.vars.keys()):
            self.vars[var_name] = default_val
    '''
    **kwargs are name, values to fill card
    card format:
        ----- card 1
        title \ has default value
        ----- card 2
        itype
        igrad
        item
        neign
        ----- card 3 skip card 3 if neign=0
        nvalu \ has default value 0
        nvctr \ has default value 0
        ----- card 4
        ieltyp
        npe
        mesh
        nprnt \ has default value 1
        ----- card 5 skip card 5 if mesh=1
        nem \ generated automatically
        nnm \ generated automatically
        ----- card 6 skip cards 6-7 if mesh!=0, read card 6 for each element
        nod \ taken from elem_connectivity
        ----- card 7 read for each node
        glxy \ taken from node_coords
        ----- card 8 skip cards 8-11 if mesh<=1
        nrecl \ generated automatically
        ----- card 9 read card 9 nrecl times
        nod1
        nodl
        nodinc
        x1
        y1
        xl
        yl
        ratio
        ----- card 10
        nrecel \ generated automatically
        ----- card 11 read card 11 nrecel times
        nel1
        nell
        ielinc
        nodinc
        npe
        nodei
        ----- card 12 skip cards 12-14 if mesh!=1
        nx \ generated automatically
        ny \ generated automatically
        ----- card 13
        x0
        dx
        ----- card 14
        y0
        dy
        ----- card 15
        nspv \ generated automatically
        ----- card 16 skip card 16 if nspv=0, repeat nspv times
        ispv
        ----- card 17 if nspv=0 or neign!=0
        vspv
        ----- card 18 skip card 18 if neign!=0
        nssv \ generated automatically
        ----- card 19 skip card 19 if nssv=0 or neign!=0
        issv
        ----- card 20 skip card 20 if nssv=0 or neign!=0
        vssv
        ----- card 21 skip cards 21-27 if itype!=0
        a10
        a1x
        a1y
        ----- card 22
        a20
        a2x
        a2y
        ----- card 23
        a00
        ----- card 24
        iconv
        ----- card 25
        nbe \ generated automatically
        ----- card 26 repeat nbe times
        ibn
        beta
        tinf
        ----- card 27 repeat nbe times
        inod
        ----- card 28 skip card 28 if itype!=1
        viscity
        penalty
        ----- card 29 skip cards 29-30 if itype!=2
        lnstrs
        ----- card 30
        e1
        e2
        anu12
        g12
        thkns
        ----- card 31 skip card 31 if itype!=3 or 5
        e1
        e2
        anu12
        g12
        g13
        g23
        thkns
        ----- card 32 skip card 32 if neign!=0
        f0
        fx
        fy
        ----- card 33 skip card 33 if item=0
        c0
        cx
        cy
        ----- card 34 skip cards 34-35 if item=0 or neign!=0
        ntime
        nstp
        intvl
        intial
        ----- card 35
        dt
        alfa
        gama
        epsln
        ----- card 36 skip card 36 if item=0 or intial=0 or neign!=0, repeat neq times
        glu
        ----- card 37 skip card 37 item<=0 or neign!=0 or intial=0, repeat neq times
        glv
        
    '''
    def __init__(self, elem_connectivity=None, node_coords=None, print_sol=True, **kwargs):
        self.vars = kwargs
        self.card = ''
        self.elem_connectivity = elem_connectivity
        self.node_coords = node_coords
        self.print_sol = print_sol
        self.solved = False
        
        # fill default values
        self.set_default('title', 'FEM 2D problem data')
        self.set_default('nvalu', 0)
        self.set_default('nvctr', 0)
        self.set_default('nprnt', 1)
        
        for k, v in self.vars.items():
            setattr(self, k, v)
    
    
    def add_card(self, *args):
        for arg in args:
            self.card += str(arg) + ' '
        self.card += '\n'
    
    def build_cards(self):
        self.card = '' # clear card
        ############ cards 1, 2
        self.add_card(self.title)
        self.add_card(self.itype, self.igrad, self.item, self.neign)
        ############ card 3
        if(self.neign != 0):
            self.add_card(self.nvalu, self.nvctr)
        ############ card 4
        self.add_card(self.ieltyp, self.npe, self.mesh, self.nprnt)
        ############ card 5
        if(self.mesh != 1):
            self.add_card(len(self.elem_connectivity), len(self.node_coords)) # nem, nnm
        ############ card 6, 7
        if(self.mesh==0):
            for elem in self.elem_connectivity:
                self.add_card(*elem)
            for elem in self.node_coords:
                self.add_card(*elem)
        ############ cards 8-11
        if(self.mesh > 1): 
            self.add_card(len(self.nod1)) # nrecl
            for i in range(len(self.nod1)):
                self.add_card(self.nod1[i], self.nodl[i], self.nodinc[i], 
                        self.x1[i], self.y1[i], self.xl[i], self.yl[i], self.ratio)
            self.add_card(len(self.nel1)) # nrecel
            for i in range(len(self.nel1)):
                self.add_card(self.nel1[i], self.nell[i], self.ielinc[i], 
                        self.nodinc[i], self.npe[i], *self.node[i])
        ############ cards 12-14
        if(self.mesh==1):
            self.add_card(len(self.dx), len(self.dy)) # nx, ny
            self.add_card(self.x0, *self.dx)
            self.add_card(self.y0, *self.dy)
        ############ cards 15-17
        if(not hasattr(self, 'ispv')):
            self.add_card(0) # nspv
        else:
            self.add_card(len(self.ispv)) # nspv
            for node in self.ispv:
                self.add_card(*node)
            if(self.neign == 0):
                for v in self.vspv:
                    self.add_card(v)
        ############ cards 18-20
        if(not hasattr(self, 'issv')):
            self.add_card(0) # nssv
        else:
            self.add_card(len(self.issv)) # nssv
            for node in self.issv:
                self.add_card(*node)
            if(self.neign == 0):
                for v in self.vssv:
                    self.add_card(v)
        ############ cards 21-27
        if(self.itype==0):
            self.add_card(self.a10, self.a1x, self.a1y)
            self.add_card(self.a20, self.a2x, self.a2y)
            self.add_card(self.a00)
            self.add_card(self.iconv)
            if(self.iconv!=0):
                if(not hasattr(self, 'ibn')):
                    self.add_card(0) # nbe
                else:
                    self.add_card(len(self.ibn)) # nbe
                    for i in range(len(self.ibn)):
                        self.add_card(self.ibn[i], self.beta[i], self.tinf[i])
                    for i in self.inod:
                        self.add_card(*self.inod)
        ############ card 28
        if(self.itype==1):
            self.add_card(self.viscsity, self.penalty)
        ############ cards 29, 30
        if(self.itype==2):
            self.add_card(self.lnstrs)
            self.add_card(self.e1, self.e2, self.anu12, self.g12, self.thkns)
        ############ card 31
        if(self.itype==3 or self.itype==5):
            self.add_card(self.e1, self.e2, self.anu12, self.g12, self.g13, self.g23, self.thkns)
        ############ card 32
        if(self.neign==0):
            self.add_card(self.f0, self.fx, self.fy)
        ############ cards 33-37
        if(self.item!=0):
            self.add_card(self.c0, self.cx, self.cy)
            if(self.neign==0):
                self.add_card(self.ntime, self.nstp, self.intvl, self.intial)
                self.add_card(self.dt, self.alfa, self.gama, self.epsln)
                if(self.intial!=0):
                    for i in self.glu:
                        self.add_card(i)
        if(self.item>0 and self.neign==0 and self.intial!=0):
            for i in self.glv:
                self.add_card(i)
        return self.card
    
    def save_card(self, filename):
        with open(filename, 'w') as f:
            f.write(self.card)
    
    def run(self):
        self.build_cards()
        self.save_card('tempcard.inp')
        # solve with subprocess
        result = subprocess.run([
            r"/mnt/3d1ece2f-6539-411b-bac2-589d57201626/home/burak/Documents/books/Introduction_to_the_FEM_Reddy/tamu/dncoble/fem2d.exe", 'tempcard.inp', 'solved_card.txt'
        ])
        # while(not os.path.exists('solved_card.txt')):
        #     pass
        with open('solved_card.txt', 'r') as f:
            solution_card = f.read()
        if(self.print_sol):
            print(solution_card)
        os.remove('tempcard.inp')
        os.remove('solved_card.txt')
        self.solved = True
        self.solution_card = solution_card
        return solution_card
    
    '''
    collect data on primary and secondary variables from the solution card
    '''
    def postprocess(self, poly_order=3):
        if(not self.solved):
            return
        i = self.solution_card.index('Primary DOF\n') + 81
        j0 = self.solution_card.index('Orientation')
        j1 = j0 - 307
        j2 = j0 + 93
        l = self.solution_card[i:j1]
        n = [j.split(' ') for j in l.split('\n')]
        m = []
        for k in n:
            r = []
            for j in k:
                if j != '':
                    r.append(float(j))
            m.append(r)
        prim_mat = np.array(m)
        l = self.solution_card[j2:-109]
        n = [j.split(' ') for j in l.split('\n')]
        m = []
        for k in n:
            r = []
            for j in k:
                if j != '':
                    r.append(float(j))
            m.append(r)
        
        sec_mat = np.array(m)

        prim_mat = prim_mat[:,1:]
        sec_mat = sec_mat[:,:4]
        
        self.poly_features = PolynomialFeatures(degree=poly_order)
        prim_features = self.poly_features.fit_transform(prim_mat[:,0:2])
        sec_features = self.poly_features.fit_transform(sec_mat[:,0:2])
        
        # create linear models and save the predict functions
        self.u = linear_model.LinearRegression().fit(prim_features, prim_mat[:,2]).predict
        self.qy = linear_model.LinearRegression().fit(sec_features, sec_mat[:,2]).predict
        self.qx = linear_model.LinearRegression().fit(sec_features, sec_mat[:,3]).predict
        
        return prim_mat, sec_mat
    
    """
    get variable from solved solution
    var: one of 'u', 'qx', 'qy'
    x: array of points
    """
    def get_var(self, var, x):
        if(not self.solved):
            return
        if(var == 'u'):
            return self.u(self.poly_features.transform(x))
        if(var == 'qy'):
            return self.qy(self.poly_features.transform(x))
        if(var == 'qx'):
            return self.qx(self.poly_features.transform(x))
