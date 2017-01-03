function demoSumprodGaussCanon
%DEMOSUMPRODGAUSSCANON Sum-Product algorithm test on canonical Gaussians

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
pot(2).table.invmean=randn(sum(globaldim(pot(2).variables)),1);
pot(2).table.invcovariance=tmp*tmp'; pot(2).table.logprefactor=0;
pot(2).table.type='GaussianCanonical';
pot(2).table.dim=globaldim(pot(2).variables);

A = FactorGraph(pot); figure; drawFG(A);
[marg mess]=sumprodFG(pot,A,[]);

jointpot = multpots(pot);
disp('Message passing on Gaussians defined using the moment representation.')
disp('Marginal distribution of each variable')
for i=1:length(variables)
    fprintf(1,'\nvariable %d:\n',i);
    margpot(i)= normpot(sumpot(jointpot,i,0));  
    disp(['mean (factor graph method): ',num2str(marg(i).table.mean')])
    disp(['mean (raw integral method): ',num2str(margpot(i).table.mean')])
    disp(['covariance(factor graph method): ',num2str(marg(i).table.covariance(:)')])
    disp(['covariance(raw integral method): ',num2str(margpot(i).table.covariance(:)')])
end