function demoMultGaussianMomentGaussianCanonical
%DEMOMULTGAUSSIANMOMENTGAUSSIANCANONICAL demo of multiplying two Gaussians,
%one in moment form, the other in canonical form

% Variable order is arbitary
variables=1:3; [a b c]=assign(variables);
globaldim=[2 3 1]; % number of dimensions of each variable

pot(1).variables=[b a]; tmp=randn(sum(globaldim(pot(1).variables)));
pot(1).table.invmean=randn(sum(globaldim(pot(1).variables)),1);
pot(1).table.invcovariance=tmp*tmp'; pot(1).table.logprefactor=0;
pot(1).table.type='GaussianCanonical';
pot(1).table.dim=globaldim(pot(1).variables);
pot(2).variables=[b c]; 

tmp=randn(sum(globaldim(pot(2).variables)));
pot(2).table.mean=randn(sum(globaldim(pot(2).variables)),1);
pot(2).table.covariance=tmp*tmp'; pot(2).table.logprefactor=0;
pot(2).table.type='GaussianMoment';
pot(2).table.dim=globaldim(pot(2).variables);

jointpot = multpots(pot); % multiply a Gaussian Canonical with a Gaussian moment (returns a Gaussian Canonical)

table(jointpot)
table(convertGaussianCanonicalToGaussianMoment(jointpot))