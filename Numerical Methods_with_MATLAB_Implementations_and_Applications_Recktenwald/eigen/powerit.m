function [lambda,v] = powerit(A,s,nit,x0,verbose)
% powerit  Shifted power method for finding matrix eigenvalues
%
% Synopsis: lam = powerit(A)                      [lam,v] = powerit(A)
%           lam = powerit(A,s)                    [lam,v] = powerit(A,s)
%           lam = powerit(A,s,nit)                [lam,v] = powerit(A,s,nit)
%           lam = powerit(A,s,nit,x0)             [lam,v] = powerit(A,s,nit,x0)
%           lam = powerit(A,s,nit,x0,nit,verbose) [lam,v] = powerit(A,s,nit,x0,verbose)
%
% Input:  A = an n by n matrix
%         s = (optional) shift parameter: Iterations involve the matrix
%             B = A - s*I.  Default:  s = 0;
%         nit = (optional) number of iterations.  Default:  nit = 10
%         x0 = (optional) n by 1 (column) vector to start the iterations
%              Default:  x0 = rand*size(A,2);
%         verbose = (optional) flag to control printing of results at each iteration.
%                   Default: verbose = 0, no printing
%
% Output:    lam = eigenvalue assuming iterations converge
%            v = (optional) eigenvector corresponding to lambda
[m,n] = size(A);
if m~=n,  error('A must be square');   end
if nargin<2,  s = 0;           end
if nargin<3,  x0 = rand(m,1);  end
if nargin<4,  nit = 10;        end
if nargin<5,  verbose = 0;     end

u = x0;
B = A - s*eye(size(A));   %  apply shift
if verbose,  fprintf('  k    norm(u,inf)\n');   end
for k=1:nit
  u = B*u;
  mu = norm(u,inf);
  u = u/mu;
  if verbose, fprintf('%3d     %f\n',k,mu);  end
end
lambda = s + mu;
if nargout==2, v = u; end
