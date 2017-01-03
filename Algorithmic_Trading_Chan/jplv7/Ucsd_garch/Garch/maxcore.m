function [e,E]=maxcore(regressand,parameters,ma,tau);
% PURPOSE:
%     Forward recursion for armax estimation
% 
% USAGE:
%     [e,E]=maxcore(regressand,parameters,ma,tau)
% 
% INPUTS:
%     See maxlikelihood or armaxlikelihood
% 
% OUTPUTS:
%     See maxlikelihood or armaxlikelihood
% 
% COMMENTS:
%     Helper function part of UCSD_GARCH toolbox. Used if you do not use the MEX file.
%     You should use the MEX file.
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

e=zeros(tau,1);
for t=ma+1:tau;
    e(t)=regressand(t)-parameters*e(t-1:-1:t-ma);
end
E=e(ma+1:tau)'*e(ma+1:tau);
