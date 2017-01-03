function [u,lambda] = iterMult(A,x,nit)
% iterMult  Iterated multiplication of a vector by a matrix: u = A*A*...*A*x
%
% Synopsis:  u          = iterMult(A,x,nit)
%            [u,lambda] = iterMult(A,x,nit)
%
% Input:     A = an n by n matrix
%            x0 = n by 1 (column) vector to start the iterations
%            nit = number of iterations
%
% Output:    u = A*A* ... A*x, result of m multiplications of A on x
%                scaled by the max norm of the result at each step
%                The max norm of u is printed at each step
%            lambda = (optional) infinity norm of the scaled u
u = x;
fprintf('  k    norm(u,inf)\n');
for k=1:nit
  u = A*u;
  r = norm(u,inf);
  u = u/r;
  fprintf('%3d     %f\n',k,r);
end
if nargout==2, lambda = r; end
