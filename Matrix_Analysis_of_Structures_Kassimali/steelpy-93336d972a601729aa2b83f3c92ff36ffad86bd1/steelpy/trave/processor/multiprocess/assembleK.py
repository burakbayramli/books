# 
# Copyright (c) 2009-2020 fem2ufo
#
# Python stdlib imports
from itertools import chain
import multiprocessing as mp
import pickle

# package imports
#from steelpy.frame3D.preprocessor.assemble import assemble
#from steelpy.frame3D.processor.operations import zeros

#
def zeros(m, n=None, code:str = 'd'):
    """
    Create zero matrix
    """
    if n: 
        new_matrix = [[0 for row in range(n)] for col in range(m)]
        #new_matrix = np.zeros((m, n), dtype=np.float64, order='F')
    else: 
        new_matrix = array(code, [0 for row in range(m)])
    
    return new_matrix
#
def assemble(idof, jdof, jbc, a, aa):
    """
    """
    # set ipv to the positions in the array of the nodes
    ipv = [int(idof + i) for i in range(6)]
    ipv.extend([int(jdof + i) for i in range(6)])
    # store the values for individual array in global array
    for i in range( 12 ):
        try:
            1.0 / jbc[ipv[i]]
            ieqn1 = jbc[ipv[i]]
            for j in range(i, 12):
                try:
                    1.0 / jbc[ipv[j]]
                    ieqn2 = jbc[ipv[j]]
                    if ieqn1 > ieqn2:
                        jband = (ieqn1 - ieqn2)
                        aa[ieqn2-1][jband] += a[i][j]
                    else:
                        jband = (ieqn2 - ieqn1)
                        aa[ieqn1-1][jband] += a[i][j]
                except ZeroDivisionError:
                    continue
            # L30:
        except ZeroDivisionError:
            continue    
#
#
def loop_members(elements, jbc, neq, iband, q):
    """
    """
    aa = zeros( neq, iband )
    #
    for memb in elements.values():
        idof, jdof = memb[0]
        a = memb[1]
        assemble(idof, jdof, jbc, a, aa)
    #return aa
    q.put(aa)
#
def main():
    """
    """
    #file = open( "jbc.p", "rb" )
    #jbc = pickle.load(file)
    #file.close()
    #
    file = open( "stffmtx.p", "rb" )
    neq = pickle.load(file)
    iband = pickle.load(file)
    #stf = pickle.load(file)
    file.close()
    #
    elements = pickle.load(open( "memb.p", "rb" ))
    jbcc = pickle.load(open( "jbc.p", "rb" ))
    jbc = list( chain.from_iterable( jbcc ) )
    
    #for memb in elements:
    #    idof, jdof = memb[0]
    #    a = memb[1]
    #    assemble(idof, jdof, jbc, a, stf)
    #
    #aa = loop_members(elements)
    #
    #with mp.Manager() as manager:
    #freeze_support()
    #manager = Manager()
    #d = manager.list(elements)
    #l = manager.list(jbc)
    #st = manager.list(stf)
    #st = Array('f', stf)
    qout = mp.Queue()
    p = mp.Process(target=loop_members, 
                   args=(elements, jbc, neq, iband, qout))
    p.start()
    try:
        stf = qout.get(False)
    except qout.empty():
        pass
    p.join()
    #
    #print(stf)
    #p = Pool(processes=2)
    #stiff = p.starmap_async(loop_members, elements)
    #p.close()
    #print(stiff.get())
    #
    #
    #file = open( "stffmtx.p", "ab" )
    #pickle.dump( p.st, file )
    #file.close()
    #
    file = open( "stffmtx.p", "ab" )
    pickle.dump( neq, file )
    pickle.dump( iband, file )
    pickle.dump( stf, file )
    file.close()     
    print('** End multiprocessing Matrix')
#

#if __name__ == 'steelpy.frame3D.preprocessor.assemblyMatrix':
if __name__ == '__main__':
    #freeze_support()
    main()   
    
