% spatial probit model with spatial individual effects
%
% semip_g.m  - matlab program to compute estimates for spatial probit model with individual effects
% semip_gc.m - matlab program that calls semip_gcc.c mex file to do estimation
% compile with: mex semip_gcc.c matrixjpl.c randomlib.c
% matrixjpl.c: matrixjpl.h support files
% randomlib.c: randomlib.h support files
% semip_gd   : demo file
% semip.mat  : data for demo file
% semip_gd   : An example of using semip_g and semip_gc
% semip_gd2  : a comparison of Metropolis-Hastings vs inversion sampling for rho
% semip_gcd  : An example of using semip_gc
% semip_gcd2 : An example of using the seed function semip_gc
