function newpot=normpotGaussianCanonical(pot)
newpot.variables=pot.variables;
newpot.table.covariance=inv(pot.table.invcovariance);
newpot.table.mean=inv(pot.table.invcovariance)*pot.table.invmean;
newpot.table.logprefactor=pot.table.logprefactor+...
0.5*pot.table.invmean'*inv(pot.table.invcovariance)*pot.table.invmean+...
0.5*logdet(2*pi*inv(pot.table.invcovariance));
