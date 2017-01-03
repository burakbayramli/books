% PURPOSE: An example of using invpd()                               
%          a function to mimic Gauss invpd
%          replaces small eigenvalues with 0.01 values
%---------------------------------------------------
% USAGE: invpd_d
%---------------------------------------------------

clear all;

format long;
n = 100;
x = randn(n,4);
x(:,4) = x(:,3) + 1e-8*rand(n,1);

xpx = x'*x;
xi = invpd(xpx);
[P,d] = modchol(xi);
xi = P'*P
xi*xpx

invpd(xpx)*xpx

inv(xpx)*xpx

% non-singular symmetric matrix
sig = [2 0 2.4
       0 2 0
     2.4 0 3];

invpd(sig)*sig
sig\sig


% more singular symmetric matrix
sig = [2.99 0 2.5
       0 2 0
     2.5 0 3];

invpd(sig)*sig
sig\sig


sig = [2 0 10
       0 2 0
     10 0 3];

invpd(sig)*sig
sig\sig

format short;

