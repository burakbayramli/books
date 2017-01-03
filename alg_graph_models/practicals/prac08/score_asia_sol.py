from gPy.Examples import asia
from gPy.Parameters import CompactFactor
from gPy.IO import read_csv
import sys

data = CompactFactor(read_csv(open(sys.argv[1])))
print asia.bdeu_score(data)


def score_adg(adg,data):
    print '^^^^'
    for child in adg.vertices():
        parents = adg.parents(child)
        family = parents | set([child])
        data_cpt = data.makeFactor(family).makeCPT(child,False)
        print child, data_cpt.bdeu_score()
    print 'vvvvv'
    print
    
adg = asia.adg()
score_adg(adg,data)
adg.remove_arrow('Smoking','Cancer')
score_adg(adg,data)
