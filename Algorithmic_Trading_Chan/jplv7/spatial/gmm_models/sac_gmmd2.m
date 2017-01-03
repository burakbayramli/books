% PURPOSE: An example of using sac_gmm on a large data set   
%          GM estimation of the general spatial model                         
%---------------------------------------------------
% USAGE: sac_gmmd2 (see sac_gmmd for a small data set)
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
W2 = make_neighborsw(latt,long,1);    % nearest neighbor-based weight matrix

vnames = strvcat('voters','const','educ','homeowners','income');

% use defaults including lndet approximation
result = sac(y,x,W,W2); % maximum likelihood estimates
prt(result,vnames);

options.iter = 1; 
result2 = sac_gmm(y,x,W,W2,options);
prt(result2,vnames);

ndraw = 2500;
nomit = 500;
prior.novi = 1;
result3 = sac_g(y,x,W,W2,ndraw,nomit,prior);
result3.tflag  = 'tstat';
prt(result3,vnames);