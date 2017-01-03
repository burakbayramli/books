function transformeddata=ivech(data)
% PURPOSE:
%     Transform a vector in to a lower triangular matrix for use by MVGARCH, complements vech
% 
% USAGE:
%     transformeddata=ivech(data)
% 
% INPUTS:
%     data:   A m by 1 vector to be transformed to a square k by k matrix.  
%            M must ba solution to the equation k^1+k-2*m=0
% 
% OUTPUTS:
%     transformeddata - a k by k lower matrix of the form 
% 
% COMMENTS:
%        [ data(1)   0           0   ...      0
%          data(2) data(k+1)     0   ...      0
%          data(3) data(k+2) data(2k) 0 ...   0
%              ...   ....      . ...    ....
%          data(k) data(2k-1) ...     data(m-1)    data(m) ]
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

t=size(data,1);
sizeout=(-1+sqrt(1+8*t))/2;
transformeddata=zeros(sizeout);
index=1;

for i=1:sizeout
    for j=i:sizeout
        transformeddata(j,i)=data(index);
        index=index+1;
    end
end


