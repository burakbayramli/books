% PURPOSE: A comparison of Bayesian and ML estimates
%          using a small dataset                 
%---------------------------------------------------
% USAGE: test_bayes
%---------------------------------------------------

clear all;

load anselin.dat; 
xc = anselin(:,4);
yc = anselin(:,5);
% crate standardized 1st-order spatial weight matrix
[j1 W j2] = xy2cont(xc,yc);
[n junk] = size(W);
IN = eye(n); 
sige = 0.25;
k = 3;
x = randn(n,k);
beta = zeros(k,1);
beta(1,1) = 1.0;
beta(2,1) = 1.0;
beta(3,1) = 1.0;
vnames = strvcat('y','x1','x2','x3');

rgrid = [-0.8 -0.4 0 0.4 0.8];

nrho = 5;
for i=1:nrho;

rho = rgrid(i);
u = (IN - rho*W)\(randn(n,1)*sqrt(sige));
y = x*beta + u;


prior.novi = 1;  % homoscedastic prior for comparison
ndraw = 3500;
nomit = 1000;
prior.lflag = 0; % full lndet calculation
info.lflag = 0; % full lndet calculation

results1 = sem_g(y,x,W,ndraw,nomit,prior);
%prt(results1,vnames);  
results2 = sem(y,x,W,info);
%prt(results2,vnames);  
results3 = sem_gmm(y,x,W);
%prt(results3,vnames);  


betas = [results1.beta results2.beta results3.beta];
rhos = [results1.rho  results2.rho results3.lambda];
rnames = strvcat('Variables');
rnames = strvcat(rnames,vnames(2:end,:));
in.rnames = strvcat(rnames,'lambda');
in.cnames = strvcat('Bayesian-SEM','Maxlik-SEM','GMM-SEM');
out = [betas
       rhos];
fprintf(1,'==== true rho = %8.4f \n',rho);
mprint(out,in);

end; % end of loop over rho values