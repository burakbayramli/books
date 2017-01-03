function heights=normal(data,center,bw);
% PURPOSE:
%     Normal kernel function
% 
% USAGE:
%     heights=triangular(data,center,bw)
% 
% INPUTS:
%     data: Data used for height calculation
%     center: The location for height calculation
%     bw:  The bandwidth
% 
% OUTPUTS:
%     heights: Scalar height for kernel density
% 
% COMMENTS:
%     For kernel density estimation
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
u=(data-center)./bw;
heights=exp(-0.5 * u .^2) ./ (sqrt(2*pi));

%((2*pi)^(-1/2))*exp((-0.5*u.^2))