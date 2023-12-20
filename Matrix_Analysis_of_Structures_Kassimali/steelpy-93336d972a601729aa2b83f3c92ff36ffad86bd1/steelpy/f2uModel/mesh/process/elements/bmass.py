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
#from steelpy.process.math.operations import to_matrix, zeros, mtxmul, trnsload_3Dv
#from steelpy.process.math.vector import Vector

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
def beam_mass(beam_length:float, section, material, ilump):
    """
    Calculate the element mass matrix
    """
    #
    length = beam_length
    #area = 0.300E-02  # self.section.area.value
    #rho = 0.200E+12  # self.material.density.value
    #zix = 0.100E-09  # self.section.Iy.value
    #
    area = section.area
    zix = section.Iy
    rho = material.density.value / 1000
    #
    if ilump == 1:
        # contribution to lumped mass matrix
        return lumped_mass_matrix(rho, area, length, zix)
    else:
        # contributions to consistent mass matrix        
        return consistent_mass_matrix(rho, area, length, zix)
#
def lumped_mass_matrix(rho, area, length, zix):
    """
    """
    em = zeros( 12, 12 )
    roal = rho * area * length / 2.0
    alpha = 1.0e-6
    em[ 0 ][ 0 ] = roal
    em[ 1 ][ 1 ] = roal
    em[ 2 ][ 2 ] = roal
    em[ 3 ][ 3 ] = roal * zix / area
    em[ 4 ][ 4 ] = roal * length * length * alpha
    em[ 5 ][ 5 ] = roal * length * length * alpha
    em[ 6 ][ 6 ] = em[ 0 ][ 0 ]
    em[ 7 ][ 7 ] = em[ 1 ][ 1 ]
    em[ 8 ][ 8 ] = em[ 2 ][ 2 ]
    em[ 9 ][ 9 ] = em[ 3 ][ 3 ]
    em[ 10 ][ 10 ] = em[ 4 ][ 4 ]
    em[ 11 ][ 11 ] = em[ 5 ][ 5 ]
    return em    
#
def consistent_mass_matrix(rho, area, length, zix):
    """
    """
    em = zeros ( 12, 12 )
    roala = rho * area * length / 6.0
    roalb = rho * area * length / 420.0
    roalc = rho * area * length / 12.0
    #
    em[ 0 ][ 0 ] = roala * 2.0
    em[ 0 ][ 6 ] = roala
    #
    em[ 1 ][ 1 ] = roalb * 156.0
    em[ 1 ][ 5 ] = roalb * 22.0 * length
    em[ 1 ][ 7 ] = roalb * 54.0
    em[ 1 ][ 11 ] = - roalb * 13.0 * length
    #
    em[ 2 ][ 2 ] = roalb * 156.0
    em[ 2 ][ 4 ] = - roalb * 22.0 * length
    em[ 2 ][ 8 ] = roalb * 54.0
    em[ 2 ][ 10 ] = roalb * 13.0 * length
    #
    em[ 3 ][ 3 ] = roala * 2.0 * zix / area
    em[ 3 ][ 9 ] = roala * zix / area
    #
    em[ 4 ][ 4 ] = roalb * 4.0 * length * length
    em[ 4 ][ 8 ] = - roalb * 13.0 * length
    em[ 4 ][ 10 ] = - roalb * 3.0 * length * length
    #
    em[ 5 ][ 5 ] = roalb * 4.0 * length * length
    em[ 5 ][ 7 ] = roalb * 13.0 * length
    em[ 5 ][ 11 ] = - roalb * 3.0 * length * length
    #
    em[ 6 ][ 6 ] = em[ 0 ][ 0 ]
    em[ 7 ][ 7 ] = em[ 1 ][ 1 ]
    em[ 8 ][ 8 ] = em[ 2 ][ 2 ]
    em[ 9 ][ 9 ] = em[ 3 ][ 3 ]
    em[ 10 ][ 10 ] = em[ 4 ][ 4 ]
    em[ 11 ][ 11 ] = em[ 5 ][ 5 ]
    em[ 7 ][ 11 ] = -em[ 1 ][ 5 ]
    em[ 8 ][ 10 ] = -em[ 2 ][ 4 ]
    #
    # impose symmetry 
    for i in range( 12 ):
        for j in range( i, 12 ):
            em[ i ][ j ] = em[ j ][ i ]    
    return em  
#