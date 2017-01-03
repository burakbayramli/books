import unittest

import sys
from gPy.Parameters import *
from gPy.Variables import clear_default_domain
from gPy.Data import *
from gPy.IO import read_csv
# places = 3 is a bit sloppy!
places = 3

class TestgPyC(unittest.TestCase):

    def setUp(self):
        clear_default_domain()
        from gPy.Examples import asia
        self.bnm = asia

        # R commands to get deal to construct matching BN
        # kslbn has uniform CPTs everywhere since it is created
        # without specifying any CPTs
        # > library(deal)
        # > data(ksl)
        # > myksl <- subset(ksl,select=c(Smok,Alc,Work,Sex,Year))
        # > kslbn <- network(myksl)
        # > kslbn <- getnetwork(insert(kslbn,2,1,nocalc=TRUE))
        # > kslbn <- getnetwork(insert(kslbn,1,3,nocalc=TRUE))
        # > kslbn <- getnetwork(insert(kslbn,4,3,nocalc=TRUE))
        
        vals = {}
        for var in 'Smok', 'Alc', 'Work', 'Sex', 'Year':
            vals[var] = ('1','2')
        # put values in default domain
        SubDomain(new_domain_variables=vals)
        smok = CPT(Factor(['Smok','Alc']),'Smok')
        alc = CPT(Factor(['Alc']),'Alc')
        work = CPT(Factor(['Work','Smok','Sex']),'Work')
        sex = CPT(Factor(['Sex']),'Sex')
        year = CPT(Factor(['Year']),'Year')
        self.cpts = (smok, alc, work, sex, year)

    def test_bde(self):
        myksl = CompactFactor(read_csv(open('myksl.dat')))

        # > ksbln.prior <- jointprior(kslbn)
        # Imaginary sample size: 64
        # > kslbn.fit <- getnetwork(learn(kslbn,myksl,ksbln.prior))

        # deal scores
        #         > score(nodes(kslbn.fit)$Smok)
        #         [1] -637.9544
        #         > score(nodes(kslbn.fit)$Alc)
        #         [1] -752.027
        #         > score(nodes(kslbn.fit)$Work)
        #         [1] -463.1716
        #         > score(nodes(kslbn.fit)$Sex)
        #         [1] -751.0772
        #         > score(nodes(kslbn.fit)$Year)
        #         [1] -666.6585
        #         > score(kslbn.fit)
        #         [1] -3270.889
        node_scores = (-637.9544,-752.027,-463.1716,-751.0772,-666.6585)
        net_score = -3270.889
        score = 0
        for i, cpt in enumerate(self.cpts):
            cpt = cpt.get_counts(myksl)
            this_score = cpt.bdeu_score(64)
            self.assertAlmostEqual(this_score,node_scores[i],places) 
            score += this_score
        self.assertAlmostEqual(score,net_score,places) 

    def test_bde2(self):
        myskl = Data(read_csv(open('myksl.dat')))

        # > ksbln.prior <- jointprior(kslbn)
        # Imaginary sample size: 64
        # > kslbn.fit <- getnetwork(learn(kslbn,myksl,ksbln.prior))

        # deal scores
        #         > score(nodes(kslbn.fit)$Smok)
        #         [1] -637.9544
        #         > score(nodes(kslbn.fit)$Alc)
        #         [1] -752.027
        #         > score(nodes(kslbn.fit)$Work)
        #         [1] -463.1716
        #         > score(nodes(kslbn.fit)$Sex)
        #         [1] -751.0772
        #         > score(nodes(kslbn.fit)$Year)
        #         [1] -666.6585
        #         > score(kslbn.fit)
        #         [1] -3270.889
        node_scores = (-637.9544,-752.027,-463.1716,-751.0772,-666.6585)
        net_score = -3270.889
        score = 0
        for i, cpt in enumerate(self.cpts):
            cpt = cpt.get_counts_sql(myskl)
            this_score = cpt.bdeu_score(64)
            self.assertAlmostEqual(this_score,node_scores[i],places) 
            score += this_score
        self.assertAlmostEqual(score,net_score,places) 

    def test_bde3(self):
        # just tests that myskl has the right counts
        myskl = Data(read_csv(open('myksl.dat')))

        # > ksbln.prior <- jointprior(kslbn)
        # Imaginary sample size: 64
        # > kslbn.fit <- getnetwork(learn(kslbn,myksl,ksbln.prior))

        # deal scores
        #         > score(nodes(kslbn.fit)$Smok)
        #         [1] -637.9544
        #         > score(nodes(kslbn.fit)$Alc)
        #         [1] -752.027
        #         > score(nodes(kslbn.fit)$Work)
        #         [1] -463.1716
        #         > score(nodes(kslbn.fit)$Sex)
        #         [1] -751.0772
        #         > score(nodes(kslbn.fit)$Year)
        #         [1] -666.6585
        #         > score(kslbn.fit)
        #         [1] -3270.889
        node_scores = (-637.9544,-752.027,-463.1716,-751.0772,-666.6585)
        net_score = -3270.889
        score = 0
        for i, cpt in enumerate(self.cpts):
            child = cpt.child()
            parents = list(cpt.variables() - set([child]))
            this_score = myskl.family_score(child,parents,64.0)
            self.assertAlmostEqual(this_score,node_scores[i],places)
            score += this_score
        self.assertAlmostEqual(score,net_score,places) 

            
    def test_call_llh(self):
        dys = self.bnm['Dyspnea'].copy()
        dys._data = [1,2,3,4,5,6.2,7,8]
        priors = dys.copy()
        dys.llh(priors)

suite = unittest.makeSuite(TestgPyC)

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite)
