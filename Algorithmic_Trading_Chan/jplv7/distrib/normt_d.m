% PURPOSE: demo of truncated normal random numbers
%
%---------------------------------------------------
% USAGE: normt_d
%---------------------------------------------------

n = 1000;
mu = ones(n,1);
sig = ones(n,1);

left = -1; 
right = 3;

x = normt_rnd(mu,sig,left,right);

hist(x);
