function heights=uniform(data,center,bw);
% PURPOSE:
%     Uniform kernel function
% 
% USAGE:
%     heights=uniform(data,center,bw)
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


u=(data-center)/bw;
heights=(1/2)*(abs(u)<1);