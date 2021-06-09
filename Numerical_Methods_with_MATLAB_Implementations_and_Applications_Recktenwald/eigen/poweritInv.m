function [lam,v] = poweritInv(A,s,nit,x0,verbose)
% poweritInv  Inverse power iterations with shift to find an eigenvalue of A
%
% Synopsis:  lam = poweritInv(A)                   [lam,u] = poweritInv(A)
%            lam = poweritInv(A,s)                 [lam,u] = poweritInv(A,s)
%            lam = poweritInv(A,s,nit)             [lam,u] = poweritInv(A,s,nit)
%            lam = poweritInv(A,s,nit,x0,verbose)  [lam,u] = poweritInv(A,s,nit,x0,verbose)
%
% Input:  A = square matrix
%         s = (optional) estimate at an eigenvalue of A.  Inverse power 
%               iterations converge to the eigenvalue nearest to rho.
%               Default:  rho = 0, used to find the smallest eigenvalue
%         nit = (optional) number of iterations;  Default:  nit = 5
%         x = (optional) n by 1 (column) vector to start the iterations
%             Default:  x = rand(m,1),  [m,n] = size(A) (nr = nc)
%         verbose = (optional) flag to control printing of results at each iteration.
%                   Default: verbose = 0, no printing
%
% Output: lam = eigenvalue of A nearest to s
%         u = eigenvector associated with lam
[m,n] = size(A);
if nargin<2,  s = 0;           end
if nargin<3,  x0 = rand(m,1);  end
if nargin<4,  nit = 10;        end
if nargin<5,  verbose = 0;     end

u = x0;
[L,U] = lu(A - s*eye(size(A)));     %  Factor shifted matrix only once

if verbose,  fprintf('  k    norm(u,inf)\n');  end
for k=1:nit
  u = U\(L\u);             %  uses triangular solves; same as solving  Ay = b
  mu = norm(u,inf);
  u = u/mu;
  if verbose,  fprintf('%3d     %f\n',k,r);  end
end
lam = s + 1/mu;            %  1/(lam-s) is eigenvalue of A^(-1)-s*I 
if nargout==2, v = u; end
