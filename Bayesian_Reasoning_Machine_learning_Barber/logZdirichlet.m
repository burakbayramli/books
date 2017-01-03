function lz=logZdirichlet(u)
%LOGZDIRICHLET  Log Normalisation constant of a Dirichlet distribution with parameter u
%  lz=logZdirichlet(u)
lz = sum(gammaln(u))- gammaln(sum(u));