function table=unityGaussianCanonical(numstates)
table.invmean =zeros(sum(numstates),1); 
table.invcovariance =zeros(sum(numstates));
table.logprefactor=0;
table.dim=numstates;