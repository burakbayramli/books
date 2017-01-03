% PURPOSE: demo of left-truncated normal random numbers
%
%---------------------------------------------------
% USAGE: normlt_d
%---------------------------------------------------

n = 1000;
mu = 2*ones(n,1);
sig = 10*ones(n,1);

left = -1; 

x = normlt_rnd(mu,sig,left);

hist(x);
