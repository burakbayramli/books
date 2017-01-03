function h=tarchcore(data,dataneg,parameters,stdEstimate,p,o,q,m,T)
% PURPOSE:
%     Forward recursion to construct h's in a TARCH model
%
% USAGE:
%     h=tarchcore(data,dataneg,parameters,stdEstimate,p,o,q,m,T);
%
% INPUTS:
%     See tarchlikelihood
%
% OUTPUTS:
%     See tarchlikelihood
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
h(1:m) = stdEstimate;
for t = (m + 1):T
    h(t) = parameters(1);
    for i=1:p
        h(t) = h(t) + parameters(i+1) * data(t-i);
    end
    for i=1:o
        h(t) = h(t) + parameters(i+p+1) * dataneg(t-i);
    end
    for i=1:q
        h(t) = h(t) + parameters(i+p+o+1) * h(t-i);
    end
end
