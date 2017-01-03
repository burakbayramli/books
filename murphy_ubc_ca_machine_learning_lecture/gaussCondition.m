function [muAgivenB, sigmaAgivenB] = gaussCondition(mu, Sigma, a, x)

D = length(mu);
b = setdiff(1:D, a);
muA = mu(a); muB = mu(b);
SAA = Sigma(a,a);
SAB = Sigma(a,b);
SBB = Sigma(b,b);
SBBinv = inv(SBB);
muAgivenB = mu(a) + SAB*SBBinv*(x(b)-mu(b));
sigmaAgivenB = SAA - SAB*SBBinv*SAB';
