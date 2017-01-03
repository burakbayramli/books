import os

def plot_beta(alpha,beta):
    out = open('tmp','w')
    print >>out, 'alpha = %f' % alpha
    print >>out, 'beta = %f' % beta
    out2 = open('tmp2','w')
    var = 1/(alpha*beta/((alpha+beta)*((alpha+beta)**2)))/50
    print >>out2, alpha/(alpha+beta) , var
    out2.close()
    print >>out,  'plot [0:1] exp(lgamma(alpha+beta) - lgamma(alpha) - lgamma(beta))*(x**(alpha - 1))*((1-x)**(beta - 1)) title "beta(%f,%f)", "tmp2" title "mean" with impulses' % (alpha,beta)
    print >>out, 'pause -1'
    out.close()
    os.system('gnuplot tmp')

