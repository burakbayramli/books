from gPy.FKMCMC import OrderEnsemble, FamilyScoreCache, Order
from gPy.LearningUtils import CausalWorld
from math import exp, log
from gPy.Utils import pairs
import sys, pickle

if len(sys.argv) < 7:
    print 'usage:',sys.argv[0],' pickled_bn instances num_runs burnin sample_every num_samples'
    sys.exit(1)

model = pickle.load(open(sys.argv[1],'r'))
num_observations = int(sys.argv[2])
num_runs = int(sys.argv[3])
burnin = int(sys.argv[4])
sample_every = int(sys.argv[5])
num_samples = int(sys.argv[6])

out=sys.stdout
sys.stdout=sys.stderr
print 'run correlation sampler: num runs =',num_runs,', instances =',num_observations
print 'run correlation sampler: burnin =',burnin,', sample every =',sample_every,
print                'num_samples =',num_samples
sys.stdout=out

# observe (gibbs sample) the model and generate the sampler
# over orders of those observations
w = CausalWorld(model)
out=sys.stdout
sys.stdout=sys.stderr
print 'observing...',
sys.stdout.flush()
w.observe(num_observations)
print 'done.'
print 'scoring...',
sys.stdout.flush()
optimal_family = FamilyScoreCache(w,model.variables(), max_potential_parents = None, best_family_scale = None)
optimal_family.from_adg(model.adg())
optimal_score = Order(optimal_family, model.topological_order()).score()
print 'optimal:',optimal_score
sys.stdout=out

# settings from FK (Mixing rate test on alarm)
ensemble = OrderEnsemble(w,model.variables(), num_runs,
        burnin = burnin,
        max_potential_parents=20, max_parents_family=3,
        max_best_families=4000, best_family_scale=log(10))

print 'run','optimal',
for i in xrange(num_runs):
    print 'order_'+str(i),
print

samples = []
for i in xrange(num_samples):
    esample = ensemble.sample(skip=sample_every)
    print i,optimal_score,
    for sample in esample:
        print sample.score(),
    print
    samples.append(esample[0:2])

print
print 'blanket_score_x','blanket_score_y'
for i, j in pairs(model.variables()):
    e_ij_x = sum([samples[k][0].markov_blanket_score(i,j) for k in xrange(num_samples)])/num_samples
    e_ij_y = sum([samples[k][1].markov_blanket_score(i,j) for k in xrange(num_samples)])/num_samples
    print e_ij_x, e_ij_y

    e_ji_x = sum([samples[k][0].markov_blanket_score(j,i) for k in xrange(num_samples)])/num_samples
    e_ji_y = sum([samples[k][1].markov_blanket_score(j,i) for k in xrange(num_samples)])/num_samples
    print e_ji_x, e_ji_y

