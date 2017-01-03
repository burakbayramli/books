function [L, D] = ltdl(Q, n)
% LTDL  factorization of Q into L^T D L

% Q     symmetric n by n matrix to be factored
% L     n by n factor matrix (strict lower triangular)
% D     diagonal n-vector

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded into MATLAB by Kai Borre 12-04-96

for i = n:-1:1
   D(i) = Q(i,i);
   L(i,1:i) = Q(i,1:i)/sqrt(Q(i,i));
   for j = 1:i-1
      Q(j,1:j) = Q(j,1:j)-L(i,1:j)*L(i,j);
   end
   L(i,1:i) = L(i,1:i)/L(i,i);
end
%%%%%%%%%%%%%%%% end ltdl.m %%%%%%%%%%%%%%%%%%%%%%%%%
