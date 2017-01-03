% PURPOSE: demonstrates the use of multilogit.m
% author: simon d.woodcock
% 9/13/2002

clear; clc;

% be sure to add the econometrics toolbox to your search path

%---- CREATE SOME DEMO DATA ----%

% specify the size of the demo 
nobs = 1000;        % number of observations
nvar = 15;          % number of covariates
numcat = 5;         % number of categories

% specify the parameter vector
% note the beta vector associated with category 0 is normalized to zero
beta = [zeros(nvar,1),ones(nvar,numcat-1)];

% specify the covariates: x must include a column of ones if there 
% is a constant term
xmat = randn(nobs,nvar-1);
x = [ones(nobs,1),xmat]; 

% generate the response variable y
xbeta = x*beta;
e = 0.1*randn(nobs,numcat);
xb = xbeta + e;
exp_xb = exp(xb);
sum_exp_xb = sum(exp_xb');
for j = 1:numcat;
    P(:,j) = exp_xb(:,j) ./ sum_exp_xb';
end;
cum_P = [cumsum(P')]';
u = rand(nobs,1);
yt = ones(nobs,1)*99;
for i = 1:nobs;
    for j = 1:numcat;
        if ((u(i,1) <= cum_P(i,j)) & (yt(i,1) == 99)) 
            yt(i,1) = j;
        end;
    end;
end;
y = yt - ones(nobs,1);      % y takes values in {0,1,2,...,numcat-1}

%---- CALL MULTILOGIT.M AND PRINT RESULTS ----%

% call multilogit using default starting values, convergence criterion, and
% maximum iterations
results = multilogit(y,x);

% assign variable and category names to arrays
vnames = strvcat('y','constant','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13','x14');
cnames = strvcat('j=0','j=1','j=2','j=3','j=4');

% print results
prt_multilogit(results,vnames,cnames)

