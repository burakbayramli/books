function [VV,LL] = eigSort(A,ascend)
% eigSort  Eigenvalue/vectors sorted in ascending or descending order
%
% Synopsis:  [V,L] = eigSort(A)
%            [V,L] = eigSort(A,ascend)
%
% Input:  A = square matrix
%         ascend = (optional) flag to select sort order.  Default: ascend = 1
%                  and eigenvalue/vector pairs are sorted in ascending
%                  order.  Set ascend not equal to 1 for descending order
%
% Output: L = matrix having eigenvalues of A on diagonal, sorted in order
%             of increasing (ascend=1) or decreasing (ascend~=1) magnitude
%         V = matrix having eigenvectors of A in columns

if nargin<2, ascend=1;  end

[V,L] = eig(A);
[junk,ia] = sort(diag(L));   %  ascending sort order stored in ia

if ascend==1
  is = ia;
else
  is = ia(length(ia):-1:1);    %  descending sort order stored in id
end

lam = diag(L);               %  extract eigenvales
L = diag(lam(is));           %  and store them in sorted order
V = V(:,is');                 %  sort columns of V

if nargout>0,  VV = V;  end
if nargout>1,  LL = L;  end
