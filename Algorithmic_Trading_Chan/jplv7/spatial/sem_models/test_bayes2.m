% PURPOSE: A comparison of Bayesian and ML estimates
%          using a large dataset                 
%---------------------------------------------------
% USAGE: test_bayes2
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 150-250 seconds
load elect.dat;                    % load data on votes
y =  log(elect(:,7)./elect(:,8));     % convert to proportions
x1 = log(elect(:,9)./elect(:,8));  % of population
x2 = log(elect(:,10)./elect(:,8));
x3 = log(elect(:,11)./elect(:,8));
latt = elect(:,5);
long = elect(:,6);
n = length(y);
x = [ones(n,1) x1 x2 x3];
clear x1; clear x2; clear x3;
clear elect;                % conserve on RAM memory

[j,W,j] = xy2cont(latt,long); % contiguity-based spatial Weight matrix

vnames = strvcat('voters','const','educ','homeowners','income');

prior.novi = 1;  % homoscedastic prior for comparison
ndraw = 5500;
nomit = 1500;
% uses default MC approximation to the log-det
results1 = sem_g(y,x,W,ndraw,nomit,prior);
%prt(results1,vnames);  
results2 = sdm_g(y,x,W,ndraw,nomit,prior);
%prt(results2,vnames);  
results3 = sar_g(y,x,W,ndraw,nomit,prior);
%prt(results3,vnames);  

% uses default MC approximation to the log-det
results4 = sem(y,x,W);
results5 = sdm(y,x,W);
results6 = sar(y,x,W);

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
