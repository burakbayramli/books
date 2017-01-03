% PURPOSE: A comparison of Bayesian and ML estimates
%          using a small dataset                 
%---------------------------------------------------
% USAGE: test_bayes
%---------------------------------------------------

clear all;

load anselin.dat; % standardized 1st-order spatial weight matrix
y = anselin(:,1);
n = length(y);
x = [ones(n,1) anselin(:,2:3)];
[n,k] = size(x);
vnames = strvcat('crime','constant','income','hvalue');

load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));

% latt = anselin(:,4);
% long = anselin(:,5);
% [junk W junk] = xy2cont(latt,long);
[n junk] = size(W);

prior.novi = 1;  % homoscedastic prior for comparison
ndraw = 5500;
nomit = 2000;
prior.lflag = 0; % full lndet calculation

results1 = sem_g(y,x,W,ndraw,nomit,prior);
%prt(results1,vnames);  
results2 = sdm_g(y,x,W,ndraw,nomit,prior);
%prt(results2,vnames);  
results3 = sar_g(y,x,W,ndraw,nomit,prior);
%prt(results3,vnames);  

info.lflag = 0; % full lndet calculation
results4 = sem(y,x,W,info);
results5 = sdm(y,x,W,info);
results6 = sar(y,x,W,info);

beta = [results1.beta results4.beta];
rhos = [results1.rho  results4.rho];
rnames = strvcat('Variables');
rnames = strvcat(rnames,vnames(2:end,:));
in.rnames = strvcat(rnames,'lambda');
in.cnames = strvcat('Bayesian-SEM','Maxlik-SEM');
out = [beta
       rhos];
mprint(out,in);

beta = [results2.beta results5.beta];
rhos = [results2.rho  results5.rho];
rnames = strvcat('Variables');
rnames = strvcat(rnames,vnames(2:end,:));
[num_names junk] = size(vnames);
for i=3:num_names;
rnames = strvcat(rnames,['W-' vnames(i,:)]);
end;
in.rnames = strvcat(rnames,'rho');
in.cnames = strvcat('Bayesian-SDM','Maxlik-SDM');

out = [beta
       rhos];
mprint(out,in);

beta = [results3.beta results6.beta];
rhos = [results3.rho  results6.rho];
rnames = strvcat('Variables');
rnames = strvcat(rnames,vnames(2:end,:));
in.rnames = strvcat(rnames,'rho');
in.cnames = strvcat('Bayesian-SAR','Maxlik-SAR');

out = [beta
       rhos];
mprint(out,in);
