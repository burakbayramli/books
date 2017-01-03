function table=unityGaussianMoment(numstates)
table.mean =zeros(sum(numstates),1); 
table.covariance =10^10*eye(sum(numstates));
table.logprefactor=0;
table.dim=numstates;