% MATLAB function: dif1d_fun.m
%
% This routine returns the forcing term for
% a one-dimensional heat diffusion problem
% that has been discretized by finite differences.
% Note that the matrix A and the vector b are pre-computed
% in the main driver routine, dif1d_main.m, and passed
% to this function.  Then, this function simply returns
% f(v) = A*v + b.  So, in reality, this function is
% not specific to 1-d diffusion.

function [f] = dif1d_fun(t, v, A, b)

f = A*v + b;
