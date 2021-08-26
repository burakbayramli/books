function [d]=LinearWeights(m,r0);
% Purpose: Compute linear weights for maximum accuracy 2m-1, 
% using stencil shifted $r_0=-1,0$ points upwind.
A = zeros(m,m); b = zeros(m,1);

% Setup linear system for coefficients
for i=1:m
  col = ReconstructWeights(m,i-1+r0);
  A(1:(m+1-i),i) = col(i:m)';
end

% Setup righthand side for maximum accuracy and solve
crhs = ReconstructWeights(2*m-1,m-1+r0);
b = crhs(m:(2*m-1))'; d = A\b;
return