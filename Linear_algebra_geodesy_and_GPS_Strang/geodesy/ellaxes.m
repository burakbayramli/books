function [a,b,phi] = ellaxes(A);
%ELLAXES  Computes the eigenvalues and -vectors
%	       of a 2 by 2 positive definite matrix.

%Kai Borre 12-19-94
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

[m,n] = size(A);
if m ~= 2 | n ~= 2
   error('Wrong dimension of matrix');
end
[V,D] = eig(A);
[lambda,k] = sort(diag(D));
if lambda(k(1)) < 0
   disp('Negative eigenvalue')
   return
end;
V = V(:,k);
if any(any(V)) == 1
   phi = atan2(V(2,1),V(1,1)); 
end
a = 1/sqrt(lambda(1));
b = 1/sqrt(lambda(2));
% phi = phi*180/pi;   % convert to degrees
%%%%%%%% end ellaxes.m	%%%%%%%%%%%%%%%%%%%%%
