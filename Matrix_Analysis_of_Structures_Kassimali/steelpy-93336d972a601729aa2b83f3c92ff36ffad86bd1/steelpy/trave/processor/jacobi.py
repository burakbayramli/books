# 
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
# Python stdlib imports
#import math
import sys
#from array import array
#
# package imports
from steelpy.utils.math.operations import to_matrix, zeros, mtxmul, trnsload
#
#
#
def jacobi(a, b, n, rtol, nsmax, ibandm, x): #, neqmas
    """
    Jacobi based on bathe pp 643-645
    """
    eigv = zeros(n)
    d = zeros(n)
    # check for zero diagonal terms
    for i in range(n):
        bii = b[i][i]
        if ibandm == 1 : 
            bii = b[i][0]
        #else : 
        #    bii = b[i][i]
        if a[i][i] > 0.0  and  abs(bii) > 0.0 :
            d[i] = a[i][i] / bii
            eigv[i] = a[i][i] / bii
        else:
            raise RuntimeError('matrices not positive definite')      
    
    # initialize the modal matrix to the unit matrix
    x = [[1.0 if i == j else 0.0 for j in range(n)]
         for i in range(n)]
    
    if n == 1 : 
        return eigv
    # set sweep counter
    nsweep = 0
    nr = n - 1
    #L40:
    while nsweep < nsmax :
        nsweep += 1
        _iter = 'off'
        # check off-diags
        eps = (0.01**nsweep)**2
        iknt = 0
        #
        for j in range(nr):
            kk = j + 1
            for k in range(kk, n):
                # check that off-diag term exceeds the threshold
                if ibandm == 1 :
                    eptola = a[j][k] * a[j][k] / (a[j][j] * a[k][k])
                    if eptola < eps : 
                        continue   # goto L210
                    akk = -b[k][0] * a[j][k]
                    ajj = -b[j][0] * a[j][k]
                    ab = a[j][j] * b[k][0] - a[k][k] * b[j][0]
                    iknt += 1
                else :
                    eptola = a[j][k] * a[j][k] / (a[j][j] * a[k][k])
                    eptolb = b[j][k] * b[j][k] / (b[j][j] * b[k][k])
                    if eptola < eps and eptolb < eps : 
                        continue # goto L210
                    akk = a[k][k] * b[j][k] - b[k][k] * a[j][k]
                    ajj = a[j][j] * b[j][k] - b[j][j] * a[j][k]
                    ab  = a[j][j] * b[k][k] - a[k][k] * b[j][j]
                    iknt += 1
                #
                radicl = (ab*ab + 4.0*akk*ajj) / 4.0
                if radicl < 0.0 :
                    raise RuntimeError('matrices not positive definite')
                    #sys.error("error 2 : matrices not positive definite")
                #
                sqch = radicl**0.50
                d1 = ab / 2.0 + sqch
                d2 = ab / 2.0 - sqch
                den = d1
                if abs(d2) > abs(d1) : 
                    den = d2
                
                try :
                    _test = 1.0 / den
                    ca =  akk /  den
                    cg = -ajj / den
                except ZeroDivisionError:
                    ca = 0.0
                    cg = -a[j][k] / a[k][k]
                #
                # do generalized rotation
                if n != 2 : 
                    jp1 = j + 1
                    jm1 = j #- 0
                    kp1 = k + 1
                    km1 = k #- 0
                    # columns
                    if jm1 > 0.0 : 
                        for i in range(jm1):
                            aj = a[i][j]
                            ak = a[i][k]
                            a[i][j] = aj + cg * ak
                            a[i][k] = ak + ca * aj
                            if ibandm != 1 :
                                bj = b[i][j]
                                bk = b[i][k]
                                b[i][j] = bj + cg * bk
                                b[i][k] = bk + ca * bj
                    #L130:
                    # rows
                    if kp1 < n :
                        for i in range(kp1, n):
                            aj = a[j][i]
                            ak = a[k][i]
                            a[j][i] = aj + cg * ak
                            a[k][i] = ak + ca * aj
                            if ibandm != 1 :
                                bj = b[j][i]
                                bk = b[k][i]
                                b[j][i] = bj + cg * bk
                                b[k][i] = bk + ca * bj
                    # L160:
                    # mixture
                    if jp1 < km1 : 
                        for i in range(jp1, km1):
                            aj = a[j][i]
                            ak = a[i][k]
                            a[j][i] = aj + cg * ak
                            a[i][k] = ak + ca * aj
                            if ibandm != 1 :
                                bj = b[j][i]
                                bk = b[i][k]
                                b[j][i] = bj + cg * bk
                                b[i][k] = bk + ca * bj
                # L190:
                # do diagonal terms
                ak = a[k][k]
                a[k][k] += 2.0 * ca * a[j][k] + ca*ca*a[j][j]
                a[j][j] += 2.0 * cg * a[j][k] + cg*cg*ak
                #
                # force off-diagonal term to exact zero
                a[j][k] = 0.0
                if ibandm == 1 :
                    bk = b[k][0]
                    b[k][0] += ca*ca*b[j][0]
                    b[j][0] += cg*cg*bk
                else :
                    bk = b[k][k]
                    b[k][k] += 2.0*ca*b[j][k] + ca*ca*b[j][j]
                    b[j][j] += 2.0*cg*b[j][k] + cg*cg*bk
                    # force off-diagonal term to exact zero
                    b[j][k] = 0.0
                # update eigenvectors  
                for i in range(n):
                    xj = x[i][j]
                    xk = x[i][k]                   
                    x[i][j] += cg*xk
                    x[i][k] += ca*xj
        #L210:
        # end sweep loop
        #
        # update after each sweep
        for i in range(n):
            if abs(a[i][i]) <= 1e-12 : 
                a[i][i] = 1e-12
            
            bii = b[i][i]
            if ibandm == 1 : 
                bii = b[i][0]
            
            if abs(bii) <= 1e-20 : 
                bii = 1e-20
            
            eigv[i] = a[i][i] / bii
        # check convergence
        for i in range(n):
            tol = abs(rtol * d[i])
            dif = abs(eigv[i] - d[i])
            if dif > tol : 
                print("goto L280 1")
                _iter = 'on'
                break
        #L240:
        if _iter == 'on':
            # update d matrix for another round
            #L280:
            znorm = 0.0
            for i in range(n):
                diff = d[i] - eigv[i]
                d[i] = eigv[i]
                znorm = max(znorm, abs(diff))
            #L290:
            #
            print("@@ diff norm:", znorm," rotns = ",iknt)
            #if nsweep < nsmax : print ("goto L40")
            continue
        # check off-diagonals
        eps = rtol**2
        for j in range(nr):
            jj = j+1
            for k in range(jj, n):
                epsa = abs(a[j][k] * a[j][k] / (a[j][j] * a[k][k]))
                #epsb = abs(b[j][k] * b[j][k] / (b[j][j] * b[k][k]))
                if ibandm == 1 : 
                    epsb = 0.0
                else : 
                    epsb = b[j][k] * b[j][k] / (b[j][j] * b[k][k])
                #
                #epsa = abs(epsa)
                #epsb = abs(epsb)
                if epsa < eps and epsb < eps :
                    continue
                #print " goto L252"
                # need more iterating 
                print ("goto L280 2")
                _iter = 'on'
                break
            #L252:
            if _iter == 'on': 
                break
        #
        #L250:
        #
        if _iter == 'on':
            # update d matrix for another round
            #L280:
            znorm = 0.0
            for i in range(n):
                diff = d[i] - eigv[i]
                d[i] = eigv[i]
                znorm = max(znorm, abs(diff) )
            #L290:
            print("@@ diff norm:", znorm," rotns = ",iknt)
            #if nsweep < nsmax : print ("goto L40")
        else:
            # scale eigenvectors
            for j in range(n):
                bbjj = b[j][j]
                if ibandm == 1 : 
                    bbjj = b[j][0]
                #else : 
                #    bbjj = b[j][j]
                bbjj = abs(bbjj)
                bb =  (bbjj)**0.50
                for k in range(n): 
                    x[k][j] /= bb  # L272:
            #L270:
            # converged   return 
            nsmax = nsweep
            print('--- converged ---')
            return eigv, x
            # close #1:exit def
        #
    #
    print('nsweep [{:}] > nsmax [{:}]'.format(nsweep, nsmax))
    #sys.exit('ERROR: matrices not positive definite')
    return eigv, x
    #
#
#   
#