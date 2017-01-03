
import sys, time

def permutations( v ):
    if len(v) == 1: return [ [v[0]] ]

    res = []
    for i in range( 0, len(v) ):
        w = permutations( v[:i] + v[i+1:] )
        for k in w:
            k.append( v[i] )
        res += w
        
    return res


n = int(sys.argv[1])
v = range(n)

t0 = time.clock()
z = permutations( v )
t1 = time.clock();

print n, t1-t0
