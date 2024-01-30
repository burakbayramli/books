# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  License: MIT License
# ***********************************
import numpy as np
from nusa import *
import itertools
from timeit import default_timer as timer

#~ def pairwise(iterable):
    # "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    #~ a, b = itertools.tee(iterable)
    #~ next(b, None)
    #~ return itertools.izip(a, b)
#~ 
#~ nelm = 10
#~ parts = np.linspace(0,1,nelm)
#~ 
#~ nodos = []
#~ for xc in parts:
    #~ cn = Node((xc,0))
    #~ nodos.append(cn)
#~ 
#~ elementos = []
#~ for x in pairwise(nodos):
    #~ ni,nj = x[0], x[1]
    #~ ce = Spring((ni,nj),1000)
    #~ elementos.append(ce)
#~ 
#~ m = SpringModel()
#~ 
#~ for n in nodos: m.add_node(n)
#~ for e in elementos: m.add_element(e)
#~ 
#~ m.add_constraint(nodos[0], ux=0)
#~ m.add_force(nodos[-1], (-10,0))
#~ 
#~ t0 = timer()
#~ m.build_global_matrix()
#~ KG1 = m.KG
#~ t1 = timer()
#~ print(t1-t0)
#~ 
#~ t0 = timer() 
#~ m._build_global_matrix()
#~ KG2 = m.KG
#~ t1 = timer()
#~ print(t1-t0)
#~ 
#~ print(np.array_equal(KG1,KG2))
