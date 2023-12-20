# 
# Copyright (c) 2009-2021 fem2ufo
#
# Python stdlib imports
#from itertools import chain
import math as math
#
# package imports
#
#
#
# -------------------------
# 
def eig_out(a, eigv, jbc, ivib, nnp, neq):
    """
    Report the mode shapes for eigenvalue problems
    """
    #
    # L82:
    #
    # open report.txt for app# end as #1
    print("CHOOSE nodal storage: ")
    print("     0 = return, 1 = mode shapes, 2 = nodal vectors, 3 = full <binary>")
    #istore = input(" --> ")
    istore = 1
    try: 
        istore = int(istore)
    except : 
        istore = 0
    #
    #iout = open('stadyn.out','a')
    #ilog = open('stadyn.log','a')
    #ilog.write(" \n")
    #ilog.write(" {:} :: istore  1 = shape, 2 = vector, 3 = Matrix <binary> \n".format(istore))
    #
    print("{:} :: istore  1 = shape, 2 = vector, 3 = Matrix <binary>".format(istore))
    
    if istore == 0 : 
        #iout.close()
        #ilog.close()        
        return
    #
    elif istore == 1 :
        print("")
        print("INPUT: No of modes to report")
        #neigen = input(" --> ")
        neigen = 5
        try: 
            neigen = int(neigen)
        except : 
            neigen = 1
        #
        if neigen  >  neq : 
            neigen = neq
        
        print(" {:} :: No of nodes".format(neigen))
        #ilog.write(" {:} :: No of nodes \n".format(neigen))
        
        #disp = np.zeros(nnp*6 +1, dtype = np.float64, order = 'F')
        disp = zeros(nnp*6)
        #dispp = rddisp(neq)
        # assign displacements to each node 
        #for idof in range(1, nnp*6 + 1):
        #    ieqnum = jbc[idof]
        #    if ieqnum > 0 : disp[idof] = dispp[ieqnum]
        #    else : disp[idof] = 0.0e0
        #L70:        
        #iout.write(" ")
        for nn in range(neigen):
            # fill in the displacement vector
            for idof in range(nnp*6):
                ieqnum = jbc[idof]
                if ieqnum > 0 : 
                    disp[idof] = a[ieqnum-1][nn]
                else : 
                    disp[idof] = 0.0
            # L70:
            
            # print the displacements 
            if ivib == 2 :
                freq = abs(eigv[nn])
                freq =  math.sqrt(freq)
                print("resonant freq: {: 1.4e} rad/s".format(freq))
                #iout.write(" \n")
                #iout.write("resonant freq: {: 1.4e} rad/s \n".format(freq))
                #
                str1 = "Mode shapes"
                out_dis(nnp, disp, str1)
            elif ivib == 1 :
                freq = eigv[nn]
                print("BUCKLING L0AD: {: 1.4e}".format(freq))
                #iout.write("BUCKLING L0AD: {: 1.4e} \n".format(freq))
                str1 = "Mode shapes"
                out_dis(nnp, disp, str1)
            # 23  format(1x, a, 3x, 1g13.5, 1x, a)
        # L20:
    #
    elif istore == 2 :
        #
        print("INPUT: No of vectors to report")
        neigen = raw_input(" --> ")
        try: neigen = int(neigen)
        except : neigen = 1  
        #
        if neigen > neq : neigen = neq
        
        ilog.write(" {:} :: No of vectors \n".format(neigen))
        print(" {:} :: No of vectors \n".format(neigen))
        #iout.write(" \n")
        iout.write("MODAL VECTORS \n")
        
        for nn in range(neigen):
            iout.write("{: 1.4e} ".format(float(eigv[nn])))
            for i in range(neq):
                iout.write("{: 1.4e} ".format(float(a[i, nn])))
            iout.write(" \n")
        # L80:
    #
    elif istore == 3 :
        for nn in range(neq):
            iout.write("{: 1.4e} ".format(float(eigv[nn])))
            for i in range(neq):
                iout.write("{: 1.4e} ".format(float(a[i, nn])))
            iout.write(" \n")
        # L84:
    # end if
    #
    #iout.close()
    #ilog.close()
    #
    print('-->')
#
#
def nodeout(_time,forc,disp,vel,acc,neq,nout,inod,idyn):
    """
    """
    # Dim inod(111,2) as integer
    # Dim  disp(neq)  as double
    # Dim  vel(neq)  as double
    # Dim  acc(neq) as double
    # Dim velout(111) as double
    #
    for n in range(nout):
        iprnode = inod[n][0]

        if inod[n][1] == 0 :
            velout[n] = disp[iprnode]
        elif inod[n][1] == 1 :
            velout[n] = vel[iprnode]
        elif inod[n][1] == 2 :
            velout[n] = acc[iprnode]
    #L72:
    # open report.txt for app# end as #1
    print ([_time, forc, velout[n]]  for n in range(1, nout))
    # 122   format(1x,8(g12.5, 1x) )
    # close #1:exit def
    # close #1:exit def
    #
    # close #1
    # end def
#
