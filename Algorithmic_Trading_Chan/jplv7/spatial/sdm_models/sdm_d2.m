% PURPOSE: An example of using sdm() on a large data set
%          max likelihood spatial Durbin model                              
%---------------------------------------------------
% USAGE: sdm_d2 (see sdm_d for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations from Pace and Barry
load elect.dat;             % load data on votes
pop = elect(:,8);
y =  log(elect(:,7)./pop);  % (logged) percentage turnout
x1 = log(elect(:,9)./pop);  % education
x2 = log(elect(:,10)./pop); % homeowners
x3 = log(elect(:,11)./pop); % income
n = length(y); 
x = [ones(n,1) x1 x2 x3  ];
xc = elect(:,5);
yc = elect(:,6);
clear x1; clear x2; clear x3;
clear elect;                % conserve on RAM memory
[j1 W j2] = xy2cont(xc,yc);

n = 3107;
vnames = strvcat('voters','constant','educ','homeowners','income');

results = sdm(y,x,W);
prt(results,vnames);


prior.novi = 1;
ndraw = 2500;
nomit = 500;

results2 = sdm_g(y,x,W,ndraw,nomit,prior);
prt(results2,vnames);


out =  [results.bstd results2.beta_std results.bstd - results2.beta_std
        results.pstd results2.rho_std  results.pstd - results2.rho_std];
    
in.cnames = strvcat('Num Hess estimates','MCMC estimates','Difference');
rnames = strvcat('Std dev');
vnamesx = strvcat('constant','educ','homeowners','income', ...
                   'W*educ','W*homeowners','W*income');
in.rnames = strvcat(rnames,vnamesx,'rho');
in.fmt = '%16.8f';

mprint(out,in);


