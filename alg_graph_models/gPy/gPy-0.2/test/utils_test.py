from gPy.Examples import minibn, asia
from gPy.Models import FR,BN
from gPy.Parameters import Factor,CPT
from gPy.Variables import Domain
from random import choice,randrange,uniform,shuffle
import operator, unittest, pickle

xor = BN(domain=Domain(), new_domain_variables={'a': [0,1], 'b':[0,1], 'c':[0,1]})
xor.add_cpts([CPT(Factor(variables=['a'], data=[0.5, 0.5]),child='a')
             ,CPT(Factor(variables=['b'], data=[0.5, 0.5]),child='b')
             ,CPT(Factor(variables=['c','a','b'], data=[1.0,0.0,0.0,1.0,0.0,1.0,1.0,0.0]),child='c')
             ])
cbn_small_names = ['xor','minibn','asia']
cbn_small_test_cases = [xor,minibn,asia]
cbn_large_names = ['alarm','insurance','carpo']
try:
    # load the pickled large Bayes nets.
    cbn_large_test_cases = map(lambda fn: pickle.load(open('networks/'+fn+'_bn.pck','r')),
                    cbn_large_names)
except:
    cbn_large_names = []
    cbn_large_test_cases = []

cbn_test_cases = cbn_small_test_cases + cbn_large_test_cases


def distribution_of(model):
    """Returns a normalised factor representing the joint instantiation
    of the model.
    """

    dist = Factor(data=[1],domain=model)
    for f in model.factors():
        dist *= f
    dist.broadcast(frozenset(model.variables()))
    dist /= dist.z()
    return dist

def same_factor(f1,f2,verbose=False, dp=5):
    """Returns True iff f1 and f2 are approximately the same factor.
    This means that they contain the same variables, and no data value
    differs after the dp decimal place.

    Keyword arguments:
    f1  The first factor
    f2  The second factor
    verbose Display diagnostics if they differ
    dp  The number of decimal places to round to.
    """
    def almost_equal(x,y):
        return round(x - y,dp) == 0

    if frozenset(f1.variables()) != frozenset(f2.variables()):
        if verbose:
            print 'different variables',f1.variables(),'!=',f2.variables()
        return False

    for i, val in enumerate(map(almost_equal, f1.data(), f2.data())):
        if not val:
            if verbose:
                print 'elements #',i,'differ',f1.data()[i],'!=',f2.data()[i]
            return False
    return True


class ManySmallModelsTest(unittest.TestCase):
    def setUp(self):
        self._num_runs = 10000
        self._num_runs = 10

    def tryModel(self, model):
        pass

    def runTest(self):
        for model in cbn_small_test_cases:
            self.tryModel(model)

        for i in xrange(self._num_runs):
            model = rand_model(bayesian=True)
            self.tryModel(model)


class ManyModelsTest(unittest.TestCase):
    def setUp(self):
        self._num_runs = 10000
        self._num_runs = 10

    def tryModel(self, model):
        pass

    def runTest(self):
        for model in cbn_test_cases:
            self.tryModel(model)

        for i in xrange(self._num_runs):
            model = rand_model(bayesian=True)
            self.tryModel(model)


def rand_model(min_vars = 3, max_vars = 5
        ,min_vals = 2, max_vals = 5, bayesian = True, **kwargs):

    vs = rand_vars(min_vars, max_vars, min_vals, max_vals)

    if not bayesian:
        return rand_fr(vs,**kwargs)

    return rand_bn(vs,**kwargs)

def rand_vars(min_vars = 3, max_vars = 5, min_vals = 2, max_vals = 5):
    vs = {}
    for i in xrange(randrange(min_vars, max_vars)):
        vs['V'+str(i)] = range(randrange(min_vals,max_vals))
    return vs

def rand_fr(vs, min_fact = 1, max_fact = 10,
                min_fact_vars = 1, max_fact_vars = 10):
    model = FR(domain = Domain(), new_domain_variables = vs)

    for i in xrange(randrange(min_fact, max_fact)):
        fv = []
        while len(fv) == 0:
            for j in xrange(randrange(min_fact_vars,
                    min(max_fact_vars,len(vs.keys())))):
                v = choice(vs.keys())
                while v in fv:
                    v = choice(vs.keys())
                fv.append(v)
            fv = tuple(fv)

        n = reduce(operator.mul, [len(vs[v]) for v in fv])
        f = Factor(variables = fv
              ,data      = rand_factor_data(n)
              ,domain    = model
              ,check     = True)
        model *= f
    return model

def rand_bn(vs, max_potential_parents = 15):
    model = BN(domain = Domain(), new_domain_variables = vs)

    for child in vs.keys():
        parents = list(model.variables())
        too_many = len(parents) - max_potential_parents
        if too_many > 0:
            for i in xrange(too_many):
                parents.remove(choice(parents))

        fv = rand_subset(parents) | set([child])
        n = reduce(operator.mul, [len(vs[v]) for v in fv])
        f = Factor(variables = fv
              ,data      = rand_factor_data(n)
              ,domain    = model
              ,check     = True
              )
        cpt = CPT(f,child,True,True)
        model *= cpt
    return model

def rand_factor(vs):
    n = reduce(operator.mul, [len(vs[v]) for v in vs])
    f = Factor(variables = vs.keys()
          ,data      = rand_factor_data(n)
          ,domain    = Domain()
          ,check     = True
          ,new_domain_variables = vs)
    return f

def rand_factor_data(n,lower_bound=0.0):
    d = []
    for i in xrange(n):
        d.append(uniform(lower_bound,1000.0))

    return d

def rand_cond(model,cond):
    var = choice(tuple(model.variables() - frozenset(cond.keys())))
    val = choice(tuple(model.values(var)))
    cond[var] = [val]

def rand_subset(ss):
    r = randrange(0,2**len(ss))

    rs = set()
    for i,s in enumerate(ss):
        if ((r >> i) & 0x1) == 0x1:
            rs.add(s)

    return rs


def generate_dense_parents(density, num_vars):
    # density is the number of incident edges on a variable
    vars = ['V'+str(i) for i in xrange(num_vars)]
    while True:
        parents = {}
        neigh = dict([(var,set()) for var in vars])
        # build child/parent relations
        for i, var in enumerate(vars):
            if len(neigh[var]) == density:
                continue
            anc = frozenset(vars[:i])
            dec = frozenset(vars[i+1:])
            neighbours = [n for n in ((anc|dec)-neigh[var]) if len(neigh[n]) < density]
            shuffle(neighbours)
            neighbours = neighbours[:density-len(neigh[var])]
            for neighbour in neighbours:
                if neighbour in anc:
                    a, b = var, neighbour
                else:
                    b, a = var, neighbour

                if a in parents:
                    parents[a].add(b)
                else:
                    parents[a] = set([b])

                neigh[a].add(b)
                neigh[b].add(a)

        for var in vars:
            if len(neigh[var]) != density:
                fail=True
                break
        else:
            fail = False
        if fail:
            continue
        return vars, parents

def generate_dense_bn(density, num_vars=8, num_vals=3):
    if density > num_vars:
        raise RuntimeError,'density must be less than number of variables'

    vars, parents = generate_dense_parents(density, num_vars)
    vals = dict([(var,frozenset([i for i in xrange(num_vals)])) for var in vars])
    bn = BN(domain = Domain(), new_domain_variables = vals)
    for child in vars:
        if child in parents:
            n = num_vals**(len(parents[child]) + 1)
        else:
            n = num_vals
            parents[child] = frozenset()

        f = Factor(variables = frozenset([child])|parents[child]
                ,data = rand_factor_data(n)
                ,domain = bn
                ,check = True
                )
        bn *= CPT(f, child, True, True)
    return bn

