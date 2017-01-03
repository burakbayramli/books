
import random as rnd

n = 1000    # total visitors
k = 100     # avg visitors per day
s = 50      # daily variation

def trial():
    visitors_for_day = [0]  # No visitors on day 0

    has_visited = [0]*n     # A flag for each visitor
    for day in range( 31 ):
        visitors_today = max( 0, int(rnd.gauss( k, s )) )

        # Pick the individuals who visited today and mark them 
        for i in rnd.sample( range( n ), visitors_today ):
            has_visited[i] = 1  

        # Find the total number of unique visitors so far
        visitors_for_day.append( sum(has_visited) )

    return visitors_for_day
    
    
for t in range( 25 ):
    r = trial()
    for i in range( len(r) ):
        print i, r[i]

    print
    print
