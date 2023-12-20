# 
# Copyright (c) 2009-2023  steelpy
#
# Python stdlib imports
from __future__ import annotations
import copy
#from itertools import chain
import pickle
#
# package imports
from steelpy.utils.math.operations import zeros, matrix_full
from ..processor.jacobi import jacobi
#from ..processor.static_solver import BAK, UDUt
#
#
#
# --------------------
# eigenvalue solver
# --------------------
#
def eigen(ibandm: int, ivib: int,  
          rtol: float = 1e-12, nsmax: int = 15):  # neqmas, x, mtx, neq, iband,
    """
    Solve eigenvalue problem using jacobi rotations
    ivib : 1 geom / 2 mass
    """
    #
    file = open( "stfmx.f2u", "rb" )
    jbc = pickle.load( file )
    stf = pickle.load( file )
    mtx =  pickle.load( file )
    file.close()
    #
    #jbc = list(chain.from_iterable(jbcc))
    neq = len(stf)
    #iband = len(stf[0])      
    #    
    # stiffness
    a = matrix_full(stf)
    #
    # mass
    if ivib == 2 :
        x = mtx  # rdmass(neqmas, iband)
        # reassign to [b] matrix 
        if ibandm == 1 :
            b = zeros(neq, neq)
            for i in range(neq):
                b[i][i] = x[i][0]
        else :
            b = matrix_full(mtx)
        #ilog.write("@@ eign:  reloaded [k] [m]  ok \n")
        print("@@ eign:  reloaded [k] [m]  ok")
    elif ivib == 1 :
        b = matrix_full(mtx)
        #ilog.write("@@ eign: raloaded [k] [g] ok  ok \n")
        print("@@ eign: raloaded [k] [g] ok")
    #
    ibandm = neq # neqmas
    #nsmax = 15
    _eigv, _x = jacobi(a, b, neq, rtol, nsmax, ibandm, x) #, neqmas
    _eigv, _x = eigsrt(_eigv, _x)
    #
    print("@@ No of sweeps {:}".format(nsmax))
    #ilog.write("@@ No of sweeps".format(nsmax))
    #
    #print("----------")
    #print eigv
    # rewind isnp
    #isnp = open('stadyn.snp','w')
    for i in range(neq):
        print("{: 1.6e} ".format(float(_eigv[i])))
        #for j in range(neq):
        #    print("{: 1.6e} ".format(float(_x[j][i])))
    #    isnp.write(" \n") 
    #
    eigenp(a, b)
    #
    return _x, _eigv
#
def eigsrt(eigv, x):
    """
    Sort the eigenvalues in ascending order
    """
    neq = len(x)
    for i in range(neq):
        k = i
        p = eigv[i]
        # search for lowest value
        for j in range(i, neq):
            if abs(eigv[j]) < abs(p) :
                k = j
                p = eigv[j]
        # re-arrange vectors 
        if k != i :
            eigv[k] = eigv[i]
            eigv[i] = p
            for j in range(neq):
                p = x[j][i]
                x[j][i] = x[j][k]
                x[j][k] = p
    #
    return eigv, x
#
#
def eigenp(a, b):
    """ """
    #file = open( "stfmx.f2u", "rb" )
    #jbc = pickle.load( file )
    #a = pickle.load( file )
    #b =  pickle.load( file )
    #file.close()
    #
    neq = len(a)
    #
    import numpy as np
    from scipy.linalg import eig    
    #
    eigv, v = eig(a, b)
    #print ("{:}".format(eigv))
    #print ("====")
    #print ("{:}".format(v))
    v1 = v.T
    x = np.dot(a,v) - eigv*np.dot(b,v)
    x = x.real
    eigv = eigv.real
    #
    print("====")
    #z = np.dot(v.T, np.dot(b, v))
    z = np.dot(np.dot(v.T, b), v)
    #print ("{}".format(z))
    #print ("====")
    x = np.array([v[:,i]/np.sqrt(abs(z[i,i])) for i in range(neq)])
    x = x.T
    #print("{}".format(x))
    #print(np.divide(v,b))
    #print([v[:,i]/np.sqrt(abs(b[i,i])) for i in range(neq)])
    #
    print("----------")
    #print(eigv)
    #print np.linalg.eigvals(a)
    #print np.linalg.eigvals(b)
    #
    eigv = np.sort(eigv)
    #
    for i in range(neq):
        print("{: 1.6e} ".format(float(eigv[i])))    
    #
    return x, eigv
#
# --------------------           
#  transient 
# --------------------                      
#
def trnsient(stf, mass, jbc, npt,  #load,
             #disp, vel, acc, fmag, olddis,
             #wk, loadin, maxnode, # damp,
             ibandm: int, 
             deltat:float = 0.1, sigma:float=0.5, alpha:float=0.25,
             dampkk: float = 0.0, dampmm: float = 0.0, dampcc:float = 0.0):
    """
    Transient  analysis by newmark time integration
    ibandm : 1-banded matrix
    dampkk : stiffness damping
    dampmm : mass damping
    dampcc : damping 
    """
    #
    ildin = maxnode*3
    iprcnt = 'ok'
    #
    # get things ready for time integration loop
    # 
    print (" ")
    print (" --> ")
    # read (*,*) deltat, npt, iprcnt
    if npt > ildin :
        print("@@ time steps npt > load size {:} > {:}".format(npt, ildin))
        #print("@@ _time steps npt > load size ", npt," > ",ildin)
        npt = ildin - 1
    # end if
    print(deltat, npt, iprcnt," ::dt no pts print")
    startt = 0
    # endt = real(npt) * deltat
    #
    # set integration constants for newmark method 
    a0 = 1.0/(alpha*deltat*deltat)
    a1 = sigma/(alpha*deltat)
    a2 = 1.0/(alpha*deltat)
    a3 = 1.0/(alpha*2.0) - 1.0
    a4 = sigma/alpha - 1.0
    a5 = (sigma/alpha - 2.0) *0.5*deltat
    a6 = (1.0 - sigma) *deltat
    a7 = sigma*deltat
    #
    # read stiffness, mass & load 
    #iwidth = iband
    #rdstff(stf, iwidth)
    #iwidth = ibandm
    #rdmass(mass,iwidth)
    #rdload(fmag)
    #
    #file = open( "stfmx.f2u", "rb" )
    #jbcc = pickle.load( file )
    #stf = pickle.load( file )
    #mass =  pickle.load( file )
    #file.close()
    #
    neq = len(stf)
    iband = len(stf[0])
    iwidth = iband
    #
    print("@@ reloaded [k] [m] {p} ok")
    #print("@@ reloaded [k] [m] {p} ok")
    #
    #
    print("@@ dammath.ping coeffs: ", dampkk, dampmm)
    idamp = 0
    damp = [dampcc for i in range(neq)]
    if dampkk > 0.0 or dampmm > 0.0 : 
        idamp = 1
        damp = [dampkk * stf[i][0] + dampmm * mass[i][0] + dampcc
                for i in range(neq)]        
    #else : 
    #    idamp = 0
    #
    #if idamp == 1 :
    #    for i in range(neq):
    #        damp[i] = dampkk * stf[i][0] + dampmm * mass[i][0] + dampcc
    #
    # form effective stiffness matrix
    if ibandm == 1 :
        for i in range(neq):
            stf[i][0] += a0 * mass[i][0]
    else :
        for i in range(neq):
            for j in range(iband):
                stf[i][j] += a0 * mass[i][j]
    
    if idamp == 1 :
        for i in range(neq):
            stf[i][0] += a1 * damp[i]
    #
    # decompose effective stiffness matrix 
    #ier1 = 0
    #UDUt(stf, neq, iband, ier1)
    stf = UDUt(stf)
    #
    #if ier1 == 0 :
    #    print("error: zero diagonal term")
    #    #close #1:exit def
    #    sys.exit("error: zero diagonal term")
    #
    #print("@@ udu: mults & divs",ier1)
    #print("@@ udu: mults & divs",ier1)
    #
    # input load history from a file and interpolate
    loadin = getload(npt, deltat) # ildin
    #
    print(" ")
    print("choose nodal output: ")
    nout = 0
    # L27:
    nout += 1
    print("type: node no |  dof  |  rate  | <<0 0 0 for No end>>")
    print(" --> ")
    # read(*,*) inode, idof, irate
    jdof = (inode-1) *6 + idof
    inod[nout][1] = jbc[jdof]
    inod[nout][2] = irate
    print(inode, idof, irate," :: node dof rate")
    print("@@ eqn no ",inod[nout][1])
    if inode != 0 :  
        print (" goto L27")
    nout -= 1
    #
    # start time integration loop
    print("@@ beginning transient analysis")
    print("@@ beginning transient analysis")
    #
    # initialize tines, displacement, velocity, accln
    print("@@ initial disps. vels and accels set=0")
    for i in range(neq):
        disp[i] = 0.0e0
        vel[i]  = 0.0e0
        acc[i]  = 0.0e0
        # read(*,*) acc(i)
    #L620:
    
    forc = loadin[1]
    nodeout(_time,forc,disp,vel,acc,neq,nout,inod,idyn)
    kount = 0
    print(" ")
    # big time loop
    for itime in range(1, npt):
        if mod((itime - 1) ,10) == 0 : 
            print(itime-1)
        _time = (itime-1) * deltat
        kount += 1
        #
        # save displacements
        olddis = copy.copy(disp)
        #for i in range(neq):
        #    olddis[i] = disp[i]
        #
        # fmag says where the load is applied
        # done this way in case distributed load applied
        load = [loadin[itime] * i for i in fmag]
        #for i in range(neq):
        #    load[i] = loadin[itime] * fmag[i]
        #
        # form effective load vector
        if idamp == 1 :
            for i in range(neq):
                atermc = a1*disp[i] + a4*vel[i] + a5*acc[i]
                load[i] += atermc * damp[i]
        #
        # the effective acceleration
        for i in range(neq):
            aterm   = a0*disp[i] + a2*vel[i] + a3*acc[i]
            disp[i] = aterm
        #
        if ibandm == 1 :
            for i in range(neq):
                load[i] += disp[i] * mass[i,1]
        else :
            abband(mass, disp, load, neq, ibandm)
        # 
        # solve for new displacements : udu al# ready obtained  
        #                             : do back - defstitution
        ier1 = 0
        #BAK(stf,load,neq,iband,wk,ier1)
        wk = BAK(stf,load)
        #
        # obtain new velocities, accelerations
        for i in range(neq):
            oldvel = vel[i]
            oldacc = acc[i]
            disp[i] = wk[i]
            acc[i] = a0*(disp[i] - olddis[i]) - a2*oldvel - a3*oldacc
            vel[i] = oldvel + a6*oldacc + a7*acc(i)
        #
        # print out nodal results
        if iprcnt == kount :
            kount = 0
            forc = loadin[itime]
            nodeout(_time, forc, disp, vel, acc, neq,nout, inod, idyn)
    #
    # L30:
    # next itime
    # bottom of _time integration loop   
    #
    # 122   format(1x,8(g12.5, 1x) )        
    # 123   format(1x, i3, 1x, 6(g12.5,1x) )
    #
    # close #1
    # end def
#
def abband(matrix, vecin, vecout, neq, iband):
    """
    Multiplies [ banded ]{vector} = {vector}
    """
    for i in range(neq):
        jlim = max(0, i - iband + 1)
        for j in range(jlim, i):
            val = vecin[j]
            vecout[i] += val * matrix[j][i - j + 1]
        #
        jlim = min(iband, neq - i + 1)
        for j in range(1, jlim):
            val = vecin[i + j - 1]
            vecout[i] += val * matrix[i][j]
    #
    return vecout
#
def getload(t1: list, f1: list, npt: int, dt: float, nmax: int):
    """
    Gets applied load history by interpolation from input
    """
    #iload = 24
    #
    # read data file
    # open report.txt for app# end as #1
    #print(" ")
    # read(*,"(1a40) ") fy12
    #print(fy12," ::filename")
    #
    # open(unit=iload, file = fy12)
    # rewind iload
    #
    # adjust _times and interpolate 
    #
    _time = 0.0
    # read(iload,*,# end=240) t1, f1
    tzero = t1
    t1 = t1 - tzero
    loadin[0] = f1
    #
    k = 0
    _time += dt
    for i in range(1, 4100):
        t0 = t1
        f0 = f1
        # read(iload,*,# end=220) t1, f1
        t1 -= tzero
        # L214:
        if k == npt : 
            continue  # print " goto L220"
        if t1  >  _time :
            xk4 = (f1 - f0) / (t1 - t0) * (_time - t0) + f0
            k += 1
            _time += dt
            loadin[k] = xk4
            print (" goto L214")
    #L210:
    #
    #L220:
    print("@@ # of force points # read ", i-1)
    #print("@@ # of force points # read ", i-1)
    #     pad reminder with zeroes
    for i in range(k+1, npt):
        loadin[i] = 0.0
    #L230:
    #
    # close (iload)
    # close #1:exit def
    #
    # L240:
    print("@@ no data in load file")
    return loadin
#
#
#
