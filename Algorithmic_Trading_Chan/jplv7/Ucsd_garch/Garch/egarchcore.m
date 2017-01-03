function h=egarchcore(data, parameters, stdEstimate,p, o, q ,m , T);
% PURPOSE:
%     Core routing for egarch(use MEX file)
% 
% USAGE:
%     h=egarchcore(data, parameters, stdEstimate,p, q ,m , T);
% 
% INPUTS:
%     See egarch
% 
% OUTPUTS:
%     See egarch
% 
% COMMENTS:
%     Helper function part of UCSD_GARCH toolbox. Used if you do not use the MEX file.
%     You should use the MEX file.
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

h=zeros(T,1);
h(1:m)=stdEstimate^2;

for t = (m + 1):T
    h(t) = exp(parameters' * [1 ; data(t-(1:p))./sqrt(h(t-(1:p))); abs(data(t-(1:o)))./sqrt(h(t-(1:o))); log(h(t-(1:q)))]);
    if h(t)==0
        h(t)==stdEstimate*.00001;
    end
end