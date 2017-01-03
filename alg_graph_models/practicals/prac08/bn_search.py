from gPy.Examples import asia # easy way to get variable values
from gPy.Data import CompactFactor
from gPy.IO import read_csv
from gPy.Graphs import ADG
import sys

ordering = ('Smoking','Cancer','Bronchitis','VisitAsia',
            'Tuberculosis','TbOrCa','XRay','Dyspnea')
data = CompactFactor(read_csv(open(sys.argv[1])))
try:
    sys.argv[2]
    verbose = True
except IndexError:
    verbose = False

def best_parents(child,candidates):
    for parents in sublist(candidates,3):
        score = data.makeFactor(parents+[child]).makeCPT(child,False).bdeu_score()
        if verbose:
            print 'Score for parents %s is %f' % (parents,score)
        try:
            if score > best_score:
                if verbose:
                    print 'New best score!'
                best_score = score
                best_parents = parents
        except NameError:
            best_score = score
            best_parents = parents
    return best_parents


def sublist(candidates,n):
    if not candidates or n == 0:
        yield []
    else:
        first = [candidates[0]]
        for sl in sublist(candidates[1:],n-1):
            yield first + sl
        for sl in sublist(candidates[1:],n):
            yield sl

            
best_adg = ADG(ordering)
for i, v in enumerate(ordering):
    if verbose:
        print 'Finding best parents for %s' % v
    parents = best_parents(v,ordering[:i])
    for parent in parents:
        best_adg.add_arrow(parent,v)

print 'Best ADG'
print '*********'
print best_adg
