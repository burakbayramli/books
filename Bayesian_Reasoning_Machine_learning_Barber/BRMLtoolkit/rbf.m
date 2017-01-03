function [y phi]=rbf(x,centres,lambdas,w)
%RBF Radial Basis function output
% [y phi]=rbf(x,centres,lambdas,w)
% Radial Basis Function output for input x
%
% Inputs:
% x : input points
% centres : basis function centres
% lambdas : basis function widths
% w : basis function weights
%
% Outputs:
% y : RBF outputs
% phi : Radial Basis Functions evaluated at x
B=size(centres,2);
t1=repmat(x,1,B)-centres;
t2=t1.^2./lambdas;
phi=exp(-t2)';
y=w'*phi;