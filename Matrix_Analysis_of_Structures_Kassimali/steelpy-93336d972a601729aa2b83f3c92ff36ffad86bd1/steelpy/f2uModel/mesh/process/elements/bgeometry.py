#
# Copyright (c) 2009-2023 fem2ufo
#
from __future__ import annotations
#
# Python stdlib imports

# package imports
import numpy as np

# ---------------------
#
# Geometry
def form_geom(npi, npj, jbc, nelt, neq, iband, wk, models):
    """
    Form the global geometric matrix [g]
    """
    #
    # open report.txt for append as #1
    # print("Choose: 0 = no echo, 1 = echo to <<stadyn.out>>")
    # iecho = input("mass --> ")
    # try: iecho = int(iecho)
    # except : iecho = 1
    #
    # print(" {:} :: echo".format(iecho))
    #
    #     initialize [kg] to zero 
    #
    geo = zeros( neq, iband )
    disp = wk  # zeros(neq)
    dispp = zeros ( nnp * 6 )

    # L30:
    # disp = rddisp(neq)
    #
    # assign displacements to each node 
    for idof in range ( nnp * 6 ):
        ieqnum = jbc[ idof ]
        try:
            1.0 / ieqnum
            dispp[ idof ] = disp[ ieqnum - 1 ]
        except ZeroDivisionError:
            continue
            # L70:
    #
    #
    # next idof
    #   
    #
    # form each element matrix, and assemble 
    # imat = open('stadyn.matt','r')
    # for i in range(1, nel):
    for key, memb in elements.items ():
        #i = _element.number - 1
        ## mat = int(keywords[0])
        #e0 = _element.material.E
        #g0 = 79.3e10  # _element.material.G
        #rho0 = _element.material.density
        ##
        #a0 = _element.section.properties.area
        #zix0 = 1.0  # _element.section.properties.Ix
        #ziy0 = _element.section.properties.Iz
        #ziz0 = _element.section.properties.Iy
        ## beta angle
        #beta0 = 0
        ##
        #i1 = _element.connectivity[ 0 ]
        #j1 = _element.connectivity[ 1 ]
        #dx = _element.nodes[ 1 ].x - _element.nodes[ 0 ].x
        #dy = _element.nodes[ 1 ].z - _element.nodes[ 0 ].z
        #dz = _element.nodes[ 1 ].y - _element.nodes[ 0 ].y
        ##
        #x1 = (dx * dx + dy * dy + dz * dz) ** 0.50
        x1 = memb.length_node2node
        #l = dx / x1
        #m = dy / x1
        #n = dz / x1
        l, m, n = memb.unit_vector
        #
        # calculate the stiffnes matrix and assemble 
        # read(imat,922,rec=i) mat,e0,g0,a0,zix0,ziy0,ziz0, rho0, beta0
        # 922     format(1x,1i4,1x,8(g10.4,1x) )
        #
        # find axial force(s) on each element
        iu = int ( npi[ i ] * 6 - 6 )
        iv = int ( npi[ i ] * 6 - 5 )
        iw = int ( npi[ i ] * 6 - 4 )
        #
        ju = int ( npj[ i ] * 6 - 6 )
        jv = int ( npj[ i ] * 6 - 5 )
        jw = int ( npj[ i ] * 6 - 4 )

        s = (-(e0 * a0 / x1) * (l * (dispp[ ju ] - dispp[ iu ])
                                + m * (dispp[ jv ] - dispp[ iv ])
                                + n * (dispp[ jw ] - dispp[ iw ])))
        #
        if nelt[ i ] == 'truss':
            s = s * 30.0 / 36.0
        eg = memb.geometric_matrix( s )
        # use trans3d to transform from local to global 
        # trans3d(l,m,n,eg,beta0)
        # assemb(geo, eg, jbc, i1, j1)
        #eg = trans_3d( l, m, n, eg, beta0 )
        # print eg
        geo = assemble_matrix( geo, eg, jbc, i1, j1 )
        #     
    # L50:
    #         
    # check for zero diag; replace with fraction of norm 
    # geonorm = 0.0
    # for i in range(neq):
    #    geonorm += abs(geo[i][0])
    geonorm = sum( [ abs ( geo[ i ][ 0 ] ) for i in range( neq ) ] )
    #
    geonorm = geonorm / neq
    for i in range( neq ):
        try:
            _test = 1.0 / geo[ i ][ 0 ]
        except ZeroDivisionError:
            geo[ i ][ 0 ] = geonorm * 1.0e-6

        # if abs(geo[i][0]) == 0.0 :
        #    geo[i][0] = geonorm * 1.0e-6
    #
    # store stiffnes matrix on disk in case
    #
    # if iecho  ==  1 :
    #    iout = open('stadyn.out','a')
    #    iout.write("\n")        
    #    iout.write("GEOMETRIC: upper banded \n")
    #    for i in range(neq):
    #        for j in range(iband):
    #            iout.write(" {:1.6e}".format(float(geo[i][j])))
    #        iout.write("\n")
    #    #
    #    iout.close()
    #    #L11:
    #   
    ibandg = iband
    # rewind(igeo)
    # igeo = open('stadyn.geo','w')
    # print neq, ibandg
    # for i in range(neq):
    #    for j in range(iband):
    #        igeo.write(" {:1.6e}".format(float(geo[i][j])))
    #    igeo.write("\n")
    # L12:
    # igeo.close()
    #                                   
    print ( "@@ formgeom formed [g] ok" )
    # ilog = open('stadyn.log','a')
    # ilog.write(" \n")
    # ilog.write("@@ formgeom formed [g] ok \n")
    # ilog.close()
    #
    #
    return geo
#
def beam_geom(length, s):
    """
    Calculates the element geometric matrix  
    """
    # initialize all eg elements to zero 
    eg = np.zeros( 12, 12 )
    emlenz = s / (30.0 * length)
    alpha = (s / length) * 1.0e-6

    if abs( alpha ) < 1.0e-10:
        alpha = 1.0e-10
    #
    eg[ 0 ][ 0 ] = alpha
    eg[ 1 ][ 1 ] = 36 * emlenz
    eg[ 2 ][ 2 ] = 36 * emlenz
    eg[ 3 ][ 3 ] = alpha
    eg[ 4 ][ 4 ] = 4.0 * emlenz * length * length
    eg[ 5 ][ 5 ] = 4.0 * emlenz * length * length
    #
    eg[ 1 ][ 5 ] = 3.0 * emlenz * length
    eg[ 2 ][ 4 ] = -3.0 * emlenz * length
    #
    eg[ 6 ][ 6 ] = eg[ 0 ][ 0 ]
    eg[ 7 ][ 7 ] = eg[ 1 ][ 1 ]
    eg[ 8 ][ 8 ] = eg[ 2 ][ 2 ]
    eg[ 9 ][ 9 ] = eg[ 3 ][ 3 ]
    eg[ 10 ][ 10 ] = eg[ 4 ][ 4 ]
    eg[ 11 ][ 11 ] = eg[ 5 ][ 5 ]
    #
    eg[ 0 ][ 6 ] = -eg[ 0 ][ 0 ]
    eg[ 1 ][ 7 ] = -eg[ 1 ][ 1 ]
    eg[ 1 ][ 11 ] = eg[ 1 ][ 5 ]
    eg[ 2 ][ 8 ] = -eg[ 2 ][ 2 ]
    eg[ 2 ][ 10 ] = eg[ 2 ][ 4 ]
    eg[ 3 ][ 9 ] = -eg[ 3 ][ 3 ]
    eg[ 4 ][ 8 ] = -eg[ 2 ][ 4 ]
    eg[ 4 ][ 10 ] = -eg[ 4 ][ 4 ] / 4.0
    eg[ 5 ][ 7 ] = -eg[ 1 ][ 5 ]
    eg[ 5 ][ 11 ] = -eg[ 5 ][ 5 ] / 4.0
    #
    eg[ 7 ][ 11 ] = -eg[ 1 ][ 5 ]
    eg[ 8 ][ 10 ] = -eg[ 2 ][ 4 ]
    # impose the symmetry 
    for i in range( 12 ):
        for j in range( i, 12 ):
            eg[ j ][ i ] = eg[ i ][ j ]
    #
    return eg
#
def beam_KG(ek, beam_length, E, A, G):
    """
    """
    Ax = A
    Asy = A
    Asz = A
    EA = E*A
    L = beam_length
    T = EA(lx)/L
    Phiy = 12*EIz / (G * Asy * L**2)
    Phiz = 12*EIy / (G * Asz * L**2)
    #
    gk = np.zeros( 12, 12 )
    #
    gk[ 0 ][ 0 ] = 0
    gk[ 1 ][ 1 ] = 6/5 + 2*Phiy + Phiy**2 / (1 + Phiy)
    gk[ 2 ][ 2 ] = 6/5 + 2*Phiz + Phiz**2 / (1 + Phiz)
    gk[ 3 ][ 3 ] = Jx/Ax
    gk[ 4 ][ 4 ] = (2*L**2/15 + L**2*Phiz/6 + L**2*Phiz**2/12) / (1+Phiz)**2
    gk[ 5 ][ 5 ] = (2*L**2/15 + L**2*Phiy/6 + L**2*Phiy**2/12) / (1+Phiy)**2
    #
    gk[ 1 ][ 5 ] =  L/10 / (1+Phiy)**2
    gk[ 2 ][ 4 ] = -L/10 / (1+Phiz)**2
    #
    gk[ 6 ][ 6 ]  = -gk[ 0 ][ 0 ]
    gk[ 7 ][ 7 ]  = - 6/5 - 2*Phiy - Phiy**2 / (1 + Phiy)
    gk[ 8 ][ 8 ]  = gk[ 2 ][ 2 ]
    gk[ 9 ][ 9 ]  = gk[ 3 ][ 3 ]
    gk[ 10 ][ 10 ] = gk[ 4 ][ 4 ]
    gk[ 11 ][ 11 ] = gk[ 5 ][ 5 ]    
    
#
#
def kg_beam(self, P: float, length: float, 
             area:float, J:float, Iy:float, Iz:float,
             Emod:float, Gmod:float):
    """
    Returns the condensed (expanded) local geometric stiffness matrix for the member.

    Parameters
    ----------
    P : number, optional
        The axial force acting on the member (compression = +, tension = -)
    """
    # Get the properties needed to form the local geometric stiffness matrix
    Ip = Iy + Iz
    A = area
    L = length
    # Create the uncondensed local geometric stiffness matrix
    kg = np.array([[0, 0,    0,     0,     0,         0,         0, 0,     0,    0,     0,         0],
                   [0, 6/5,  0,     0,     0,         L/10,      0, -6/5,  0,    0,     0,         L/10],
                   [0, 0,    6/5,   0,     -L/10,     0,         0, 0,     -6/5, 0,     -L/10,     0],
                   [0, 0,    0,     Ip/A,  0,         0,         0, 0,     0,    -Ip/A, 0,         0],
                   [0, 0,    -L/10, 0,     2*L**2/15, 0,         0, 0,     L/10, 0,     -L**2/30,  0],
                   [0, L/10, 0,     0,     0,         2*L**2/15, 0, -L/10, 0,    0,     0,         -L**2/30],
                   [0, 0,    0,     0,     0,         0,         0, 0,     0,    0,     0,         0],
                   [0, -6/5, 0,     0,     0,         -L/10,     0, 6/5,   0,    0,     0,         -L/10],
                   [0, 0,    -6/5,  0,     L/10,      0,         0, 0,     6/5,  0,     L/10,      0],
                   [0, 0,    0,     -Ip/A, 0,         0,         0, 0,     0,    Ip/A,  0,         0],
                   [0, 0,    -L/10, 0,     -L**2/30,  0,         0, 0,     L/10, 0,     2*L**2/15, 0],
                   [0, L/10, 0,     0,     0,         -L**2/30,  0, -L/10, 0,    0,     0,         2*L**2/15]])
    
    kg = kg*P/L
    return kg