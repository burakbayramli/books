% PURPOSE:
%     Inverse vec_h whe using a correlation matrix(i.e. ones on diag)
% 
% USAGE:
%     Correlation=ccivech(parameters,numseries)
% 
% INPUTS:
%     parameters   - k*(k-1)/2 parameters for the correlation
%     numseries    - k, the number of series
% 
% OUTPUTS:
%     Correlation  - A k by k matrix of correlations
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


function Correlation=ccivech(parameters,numseries)
Correlation=diag(ones(numseries,1));
index=1;
for i=1:numseries
    for j=i+1:numseries
        Correlation(i,j)=parameters(index);
        Correlation(j,i)=Correlation(i,j);
        index=index+1;
    end
end
