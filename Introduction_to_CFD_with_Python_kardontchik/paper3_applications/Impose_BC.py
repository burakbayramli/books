"""
   Impose_BC

"""
import numpy as np
from numpy import pi as pi

def impose_BC(BC_points,BC_type,p,tri,boundary_nodes,boundary,tghost):
    tghost_BC = np.zeros(len(tghost),dtype=int)
    TOL = 1.0e-8

    sep_v = np.array([-999.9,-999.9])
    ind_sep = np.where(BC_points[:,0] == sep_v[0])

    # -----------------------------------------------------
    #       find nodes not in tri
    # -----------------------------------------------------
    improper = []
    for i in range(len(p)):
        ind0 = np.where(tri[:,0] == i)
        ind1 = np.where(tri[:,1] == i)
        ind2 = np.where(tri[:,2] == i)
        aa = list(np.concatenate((ind0[0],ind1[0],ind2[0])))
        if len(aa) == 0:  # node not found
            improper += [i]
    # improper consists of the list of nodes not in tri

    # -----------------------------------------------------
    #       end of correct
    # -----------------------------------------------------
    
    if len(ind_sep[0]): # found the sep_b node: we have two boundaries
        # find indices j of nodes p[j] corresponding to the external BC_points
        index = int(ind_sep[0])
        ext_BC_points = BC_points[0:index]
        
        ind = []
        for i in range(len(ext_BC_points)):
            node = np.where((p[:,0] - ext_BC_points[i,0])**2 + (p[:,1] - ext_BC_points[i,1])**2 \
                           < TOL)
            if len(node[0]) == 1: # is a proper node
                ind.append(node[0][0])
            else: # append only the proper node
                # find the proper node and append it
                for k in range(len(node[0])):
                    if node[0][k] not in improper:
                        ind.append(node[0][k])

        # find the number of boundary bars ('boundary') between the BC_points
        # and, accordingly, impose the BC on the corresponding ghost triangles
        for i in range(len(ind) - 1):
            ind1 = np.where(boundary[:,0] == ind[i])
            ind2 = np.where(boundary[:,1] == ind[i+1])
            tghost_BC[ind1[0][0]:ind2[0][0]+1] = BC_type[i]

        # find indices j of nodes p[j] corresponding to the internal BC_points
        sep_t = -1
        ind_sep_t = np.where(BC_type == sep_t)

        int_BC_points = BC_points[index+1:]
        
        ind = []
        for i in range(len(int_BC_points)):
            node = np.where((p[:,0] - int_BC_points[i,0])**2 + (p[:,1] - int_BC_points[i,1])**2 \
                           < TOL)
            if len(node[0]) == 1: # is a proper node
                ind.append(node[0][0])
            else: # append only the proper node
                # find the proper node and append it
                for k in range(len(node[0])):
                    if node[0][k] not in improper:
                        ind.append(node[0][k])

        # find the number of boundary bars ('boundary') between the BC_points
        # and, accordingly, impose the BC on the corresponding ghost triangles
        for i in range(len(ind) - 1):
            ind1 = np.where(boundary[:,0] == ind[i])
            ind2 = np.where(boundary[:,1] == ind[i+1])
            tghost_BC[ind1[0][0]:ind2[0][0]+1] = BC_type[ind_sep_t[0] + 1 + i]          
        return tghost_BC
    
    else: # there is only one boundary
        # find indices j of nodes p[j] corresponding to the BC_points
        ind = []
        for i in range(len(BC_points)):
            node = np.where((p[:,0] - BC_points[i,0])**2 + (p[:,1] - BC_points[i,1])**2 \
                           < TOL)
            if len(node[0]) == 1: # is a proper node
                ind.append(node[0][0])
            else: # append only the proper node
                # find the proper node and append it
                for k in range(len(node[0])):
                    if node[0][k] not in improper:
                        ind.append(node[0][k])

        # find the number of boundary bars ('boundary') between the BC_points
        # and, accordingly, impose the BC on the corresponding ghost triangles
        for i in range(len(ind) - 1):
            ind1 = np.where(boundary[:,0] == ind[i])
            ind2 = np.where(boundary[:,1] == ind[i+1])
            tghost_BC[ind1[0][0]:ind2[0][0]+1] = BC_type[i]
        return tghost_BC

