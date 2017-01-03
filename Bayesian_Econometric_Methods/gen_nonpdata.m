%This m-file generates the 
%experimental data for the nonparametric regression problem. 

clear;
clc;
randn('seed',sum(100*clock));
rand('seed',sum(100*clock));
%-------------------
%GENERATE THE DATA
%-------------------

nobs = 200;
x = 4*rand(nobs,1)-2;
sigeps = .01;
fx = .15*x + .3*exp(-4*((x+1).^2)) + .7*exp( -16*((x-1).^2) );
y = fx + sqrt(sigeps)*randn(nobs,1);
save nonparm_data x y;