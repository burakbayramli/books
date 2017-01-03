from gPy.LearningUtils import *
from gPy.Examples import asia
from gPy.PC import *
from gPy.Models import CBN

# generate 500 samples of the Asia network
data = CausalWorld(asia)
data.observe(500)

# for comparison
true_model = asia
true_adg = asia.adg()

# search for the ADG
## the PC algorithm consists of a search for independence relations using a
## test for conditional independence.  In this case, the G-squared statistic is
## taken to be distributed as Chi-squared (this is only true asymptotically).

ci = PCCI(G2Separator(data))

## the IC algorithm of Verma & Pearl is used to resolve these conditional
## independencies into a PDAG. Note that this may fail if the found conditional
## independencies are inconsistent with an ADG!

pdag = ICPattern(ci)

## Meek's algorithm is used to resolve this PDAG into a particular ADG

found_adg = pdag.orient()

## or for short:
## found_adg = ICPattern(PCCI(G2Separator(data))).orient()
##
## Note that G2Separator can be replaced with X2Separator or GraphSeparator
## and PCCI can be replaced with GraphCI.  The PDAG is the essential graph
## representing the Markov equivalence class of the conditional independencies.
## (This perhaps goes some way to explaining the complexity --- it depends what
## you are interested in.)
## 
## Note also that you may want to scale the p-value with the sample size and
## that there is a BFSeparator but this is likely mathematically unsound.

# fit a model to the data using the ADG
found_model = CBN.from_adg_data(found_adg, data)

print 'Structural Hamming distance:',shd(found_adg,true_adg)
print 'BDeu scores of fitted models of'
print 'found ADG:', bdeu(found_adg,data)
print ' true ADG:', bdeu(true_adg,data)
print 'KL-divergence of fitted found model from true model:',dkl(true_model,found_model)

