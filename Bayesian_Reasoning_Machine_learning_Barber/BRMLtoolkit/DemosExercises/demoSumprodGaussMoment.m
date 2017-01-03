function demoSumprodGaussMoment
%DEMOSUMPRODGAUSSMOMENT Sum-Product algorithm test for Gaussians in moment form

% Variable order is arbitary
variables=1:3; [a b c]=assign(variables);
globaldim=[2 1 3]; % dimensions for each variable
varinf(a).name='a'; varinf(b).name='b';varinf(c).name='c';

% setup the potentials:
pot(1).variables=[a b]; tmp=randn(sum(globaldim(pot(1).variables))); 
pot(1).table.mean=randn(sum(globaldim(pot(1).variables)),1);
pot(1).table.covariance=tmp*tmp'; pot(1).table.logprefactor=0;
pot(1).table.dim=globaldim(pot(1).variables);
pot(1).table.type='GaussianMoment';
pot(2).variables=[b c]; tmp=randn(sum(globaldim(pot(2).variables))); 
pot(2).table.mean=randn(sum(globaldim(pot(2).variables)),1);
pot(2).table.covariance=tmp*tmp'; pot(2).table.logprefactor=0;
pot(2).table.type='GaussianMoment';
pot(2).table.dim=globaldim(pot(2).variables);

% perform marginal inference:
A = FactorGraph(pot); subplot(1,2,1); drawNet(dag(pot),varinf); subplot(1,2,2); drawFG(A,varinf);
[marg mess]=sumprodFG(pot,A,[]);

jointpot = multpots(pot); % check: we can marginalise this joint to compute the individual marginals
disp('Message passing on Gaussians defined using the moment representation.')
disp('Marginal distribution of each variable')
for i=1:length(variables)
    fprintf(1,'\nvariable %s:\n',varinf(i).name);
    margpot(i)= normpot(sumpot(jointpot,i,0));  
    disp(['mean (factor graph method): ',num2str(marg(i).table.mean')])
    disp(['mean (raw integral method): ',num2str(margpot(i).table.mean')])
    disp(['covariance(factor graph method): ',num2str(marg(i).table.covariance(:)')])
    disp(['covariance(raw integral method): ',num2str(margpot(i).table.covariance(:)')])
end