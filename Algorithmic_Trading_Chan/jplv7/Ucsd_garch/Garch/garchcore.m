function h=garchcore(data,parameters,covEst,p,q,m,T)
% PURPOSE:
%     Forward recursion to construct h's
% 
% USAGE:
%     h=garchcore(data,parameters,covEst,p,q,m,T);
% 
% INPUTS:
%     See garchlikelihood
% 
% OUTPUTS:
%     See garchlikelihood
% 
% COMMENTS:
%     Helper function part of UCSD_GARCH toolbox. Used if you do not use the MEX file.
%     You should use the MEX file.
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
h=zeros(size(data));
h(1:m)=covEst;
for t = (m + 1):T
   h(t) = parameters' * [1 ; data(t-(1:p)).^2;  h(t-(1:q)) ];
end