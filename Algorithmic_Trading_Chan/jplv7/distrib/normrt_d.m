% PURPOSE: demo of right-truncated normal random numbers
%
%---------------------------------------------------
% USAGE: normrt_d
%---------------------------------------------------

n = 1000;
mu = 2*ones(n,1);
sig = 10*ones(n,1);

right = 4; 

x = normrt_rnd(mu,sig,right);

hist(x);
