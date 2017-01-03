function trandformeddata=vech(x)
% PURPOSE:
%        Transform a k by k matrix into a vector of size k*(k+1)/2 by 1, complements ivech
% 
% USAGE:
%      transformeddata=vech(data)
% 
% 
% INPUTS:
%      data:   A k by k matrix
% 
% 
% OUTPUTS:
%      transformeddata - a k*(k+1)/2 by 1 vector for the form
%        [data(1,1) data(2,1) ... data(k,1) data(2,2)...data(k,2)...data(k,k)]'
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

trandformeddata=x(logical(tril(ones(size(x)))));
