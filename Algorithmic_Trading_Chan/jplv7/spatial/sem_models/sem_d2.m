% PURPOSE: An example of using sem() on a large data set   
%          Gibbs sampling spatial autoregressive model                         
%---------------------------------------------------
% USAGE: sem_d2 (see sem_d for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 150-250 seconds
load elect.dat;                    % load data on votes
y =  (elect(:,7)./elect(:,8));     % convert to proportions
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

% use defaults including lndet approximation
result = sem(y,x,W); % maximum likelihood estimates
prt(result,vnames);

info.eigs = 0; % compute minimum and maximum eigenvalues of W 
               % to determine bounds on rho
info.lflag = 0; % use full log-determinant
result2 = sem(y,x,W,info); % maximum likelihood estimates
prt(result2,vnames);

