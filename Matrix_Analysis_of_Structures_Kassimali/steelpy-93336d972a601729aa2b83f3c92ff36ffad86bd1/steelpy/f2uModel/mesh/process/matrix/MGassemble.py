# 
# Copyright (c) 2009-2023 fem2ufo
#
# Python stdlib imports
from __future__ import annotations
#
from itertools import chain
import pickle

# package imports
import numpy as np
#from ..preprocessor.assemble import assemble
from steelpy.process.math.operations import to_matrix, zeros, mtxmul, trnsload_3Dv
from steelpy.process.math.vector import Vector

# ---------------------
#
# Mass
def form_mass(elements, nodes, boundaries,
              ilump: int = 1):
    """
    Form the global mass matrix [m]
    ilump [1 = lumped, 2 = consist]
    """
    #
    #
    # open report.txt for append as #1
    print ( "choose mass: 1 = lumped, 2 = consist" )
    # ilump = input("mass --> ")
    #ilump = 1
    #try:
    #    ilump = int( ilump )
    #except:
    #    ilump = 1
    #
    print ( "choose echo: 1 = echo to <<stadyn.out>>" )
    # iecho = raw_input("echo --> ")
    iecho = 1
    try:
        iecho = int( iecho )
    except:
        iecho = 1
    print ( "{:} {:} :: 1 = lumped   1 = echo".format ( ilump, iecho ) )
    #
    file = open( "stfmx.f2u", "rb" )
    jbc = pickle.load( file )
    stf = pickle.load( file )
    file.close()
    #
    #jbc = list(chain.from_iterable(jbcc))
    neq = len(stf)
    iband = len(stf[0])    
    #
    #jbcc, neq = shape_cond( elements, nodes, boundaries )
    #iband = max_bandwidth( elements, nodes, jbcc )    
    #jbc = list( chain.from_iterable( jbcc ) )
    #
    # initialize [m] to zero
    mss = zeros(neq, iband)
    #
    # form each element matrix, and assamble 
    # imat = open('stadyn.matt','r')
    #
    for key, memb in elements.items():
        em = memb.mass_matrix
        idof, jdof = memb.DoF
        #assemble(idof, jdof, jbc, em, mss)
        #ipv = Vector(jbc[idof]) + Vector(jbc[jdof])
        ipv = jbc[idof] + jbc[jdof]
        assemble(ipv, em, mss)
    #
    # L50:
    #  
    #  modify mass matrix for concentrated masses
    # rewind(icms)
    # icms = open('stadyn.tmp','r')
    #
    # for i in range(1, neq):
    # i = 0
    # for line in icms:
    #    # read(icms,*) concms
    #    keywords = line.split()
    #    concms = float(keywords[0])
    #    mss[i][0] = mss[i][0] + concms
    #    i +=1
    # L60:
    # icms.close()
    #                                 
    # store mass matrices on disk 
    #
    # if iecho == 1 :
    #    iout = open('stadyn.out','a')
    #    iout.write("\n")
    #    
    #    if ilump == 1 :
    #        iout.write(" MASS:  diagonal \n") 
    #        for i in range(neq):
    #            iout.write(" {:1.6e}".format(float(mss[i][0])))
    #    else :
    #        iout.write(" MASS: upper banded \n")
    #        for i in range(neq):
    #            for j in range(iband):
    #                iout.write(" {:1.6e}".format(float(mss[i][j])))
    #            iout.write("\n")
    #        #L11:
    #    iout.close()
    #
    ibandm = iband
    if ilump == 1:
        ibandm = 1

    # rewind(imss)
    # imss = open('stadyn.mass','w')
    # imss.write(" {} {} \n".format(neq, ibandm))
    # if ilump == 1 :
    #    for i in range(neq):
    #        imss.write(" {:1.6e}".format(float(mss[i][0])))
    # else :
    #    for i in range(neq):
    #        for j in range(iband):
    #            imss.write(" {:1.6e}".format(float(mss[i][j])))
    #        imss.write("\n")
    #    #L12:
    # end if
    # imss.close()
    #
    print ( "@@ formmass: formed [m] ok" )
    #
    # ilog = open('stadyn.log','a')
    # ilog.write(" \n")
    # ilog.write("@@ formmass: formed [m] ok \n")
    # ilog.close()
    #
    with open("stfmx.f2u", "wb") as f:
        pickle.dump(jbc, f)
        pickle.dump(stf, f)        
        pickle.dump(mss, f)    
    #
    return mss, ibandm
#
#
# --------------------
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
#