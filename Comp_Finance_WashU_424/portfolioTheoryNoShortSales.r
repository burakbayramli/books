# portfolioTheoryNoShortSales.r
#
# Examples used in Portfolio Theory with Matrix Algebra Chapter
#
# Author: Eric Zivot
# Created: August 7, 2012
# Revision history:
options(digits=4, width=70)
library(PerformanceAnalytics)
library(quadprog)
# load portfolio theory functions
# source portfolio functions
source(file="http://faculty.washington.edu/ezivot/econ424/portfolio.r")

################################################################################
# 3 asset example
################################################################################
asset.names <- c("MSFT", "NORD", "SBUX")
mu.vec = c(0.0427, 0.0015, 0.0285)
names(mu.vec) = asset.names
sigma.mat = matrix(c(0.0100, 0.0018, 0.0011,
                     0.0018, 0.0109, 0.0026,
                     0.0011, 0.0026, 0.0199),
                   nrow=3, ncol=3)
dimnames(sigma.mat) = list(asset.names, asset.names)
mu.vec
sigma.mat 
sd.vec = sqrt(diag(sigma.mat))
#
# global minimum variance portfolio with no-short sales
#

# unconstrained solution
gmin.port = globalMin.portfolio(mu.vec, sigma.mat)
gmin.port

# set restriction matrices 
D.mat = 2*sigma.mat
d.vec = rep(0, 3)
A.mat = cbind(rep(1,3), diag(3))
b.vec = c(1, rep(0,3))
D.mat
d.vec
A.mat
b.vec

# use solve.QP to minimize portfolio variance
args(solve.QP)
qp.out = solve.QP(Dmat=D.mat, dvec=d.vec,
                  Amat=A.mat, bvec=b.vec, meq=1)
class(qp.out)
names(qp.out)
qp.out$solution
sum(qp.out$solution)
qp.out$value
# compute mean, variance and sd
w.gmin.ns = qp.out$solution
names(w.gmin.ns) = names(mu.vec)
w.gmin.ns
er.gmin.ns = as.numeric(crossprod(w.gmin.ns, mu.vec))
er.gmin.ns
var.gmin.ns = as.numeric(t(w.gmin.ns)%*%sigma.mat%*%w.gmin.ns)
var.gmin.ns
sqrt(var.gmin.ns)



# compute and plot efficient frontier with short-sales
ef <- efficient.frontier(mu.vec, sigma.mat, alpha.min=0, 
                         alpha.max=1, nport=10)
ef$weights
plot(ef$sd, ef$er, type="b", ylim=c(0.02, 0.05), xlim=c(0.06, 0.11), 
     pch=16, col="blue", cex=2, ylab=expression(mu[p]), xlab=expression(sigma[p]))
points(sd.vec, mu.vec, pch=16, col="black", cex=2)
text(sd.vec, mu.vec, labels=asset.names, pos=4)

# compute efficient frontier with no-short sales
mu.vals = seq(er.gmin.ns, max(mu.vec), length.out=10)
w.mat = matrix(0, length(mu.vals), 3)
sd.vec = rep(0, length(sd.vec))
colnames(w.mat) = names(mu.vec)
D.mat = 2*sigma.mat
d.vec = rep(0, 3)
A.mat = cbind(mu.vec, rep(1,3), diag(3))
for (i in 1:length(mu.vals)) {
  b.vec = c(mu.vals[i],1,rep(0,3))
  qp.out = solve.QP(Dmat=D.mat, dvec=d.vec,
                    Amat=A.mat, bvec=b.vec, meq=2)
  w.mat[i, ] = qp.out$solution
  sd.vec[i] = sqrt(qp.out$value)
}
w.mat

points(sd.vec, mu.vals, type="b", pch=16, col="red", cex=1.5)

# illustrate infeasible portfolio
# set target return equal to 0.08
b.vec = c(0.08,1,rep(0,3))
qp.out = solve.QP(Dmat=D.mat, dvec=d.vec,
                  Amat=A.mat, bvec=b.vec, meq=2)
