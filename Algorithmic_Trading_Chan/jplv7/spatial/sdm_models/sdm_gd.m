% PURPOSE: An example of using sdm_g() Gibbs sampling
%          spatial durbin model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: sdm_gd (see also sdm_gd2 for a large data set)
%---------------------------------------------------

clear all;


% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat; 
% 5 columns:
% column1 = crime
% column2 = household income
% column3 = house values
% column4 = latitude coordinate
% column5 = longitude coordinate

n = length(anselin);
y = anselin(:,1);
x = [ones(n,1) anselin(:,2:3)]; 
latt = anselin(:,4);
long = anselin(:,5);
vnames = strvcat('crime','constant','income','hvalue');

W = make_neighborsw(latt,long,6);


% Gibbs sampling function heteroscedastic prior
% to maximum likelihood estimates
ndraw = 1250;
nomit = 250;
prior.rval = 4;
results = sdm_g(y,x,W,ndraw,nomit,prior);
prt(results,vnames);



prior2.novi = 1; % homoscedastic model
% uses default numerical integration for rho
results2 = sdm_g(y,x,W,ndraw,nomit,prior2);
prt(results2,vnames);


[h1,f1,y1] = pltdens(results.pdraw);
[h2,f2,y2] = pltdens(results2.pdraw);
plot(y1,f1,'.r',y2,f2,'.g');
legend('heteroscedastic','homoscedastic');
title('posterior distributions for rho');
xlabel('rho values');

probs = model_probs(results,results2);

fprintf(1,'posterior model probabilities \n');
in2.rnames = strvcat('Models','heteroscedastic model','homoscedastic model');
in2.cnames = strvcat('Posterior model probabilities');
in2.fmt = '%12.6f';

mprint(probs,in2);


