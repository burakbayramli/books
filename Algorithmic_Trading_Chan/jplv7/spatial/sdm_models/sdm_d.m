% PURPOSE: An example of using sdm() max likelihood
%          estimation of the spatial durbin model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: sdm_d (see also sdm_d2 for a large data set)
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

info.lflag = 0;
info.rmin = -1;
info.rmax = 1;
results = sdm(y,x,W,info);
prt(results,vnames);


prior.lflag = 0;
prior.novi = 1;
ndraw = 2500;
nomit = 500;

results2 = sdm_g(y,x,W,ndraw,nomit,prior);
prt(results2,vnames);


out =  [results.bstd results2.beta_std results.bstd - results2.beta_std
        results.pstd results2.rho_std  results.pstd - results2.rho_std];
    
in.cnames = strvcat('Hessian estimates','MCMC estimates','Difference');
rnames = strvcat('Std dev');
vnamesx = strvcat('constant','income','hvalue','W*income','W*hvalue');
in.rnames = strvcat(rnames,vnamesx,'rho');
in.fmt = '%16.8f';

mprint(out,in);


