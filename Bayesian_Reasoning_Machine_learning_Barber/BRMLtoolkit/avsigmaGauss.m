function out=avsigmaGauss(mn,v)
%AVSIGMAGAUSS Average of a logistic sigmoid under a Gaussian
% out=avsigmaGauss(mn,v)
% simple approximation for the average of 1/(1+exp(-x)) over a Gaussian
% with mean mn and variance v
erflambda=sqrt(pi)/4;
out=0.5+0.5*erf(erflambda*mn/sqrt(1+2*erflambda^2*v));

