function [L,D] = ldldecom(Q)
%LDLDECOM: Find LtDL-decompostion of Q-matrix
%
% This routine finds the LtDL decomposition of a given variance/
% covariance matrix.
%
% Input arguments:
%    Q: Symmetric n by n matrix to be factored
%
% Output arguments:
%    L: Out - n by n factor matrix (strict lower triangular)
%    D: Out - Diagonal n-vector

% ----------------------------------------------------------------------
% File.....: ldldecom
% Date.....: 19-MAY-1999
% Author...: Peter Joosten
%	     Mathematical Geodesy and Positioning
%	     Delft University of Technology
% ----------------------------------------------------------------------

global D

n = size (Q,1);

for i = n:-1:1;

   D(i) = Q(i,i);
   L(i,1:i) = Q(i,1:i)/sqrt(Q(i,i));

   for j = 1:i-1
      Q(j,1:j) = Q(j,1:j)-L(i,1:j)*L(i,j);
   end

   L(i,1:i) = L(i,1:i)/L(i,i);

end;

if (sum(D < 1E-10));

  error ('Matrix on input is not positive definite!');

end;

% ----------------------------------------------------------------------
% End of routine: ldldecom
% ----------------------------------------------------------------------
