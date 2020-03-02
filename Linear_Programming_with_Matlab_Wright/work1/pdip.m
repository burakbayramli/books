function [x,y,s,f] = pdip(A,b,p)
% primal-dual interior-point method for problem 
%
% min p'x s.t. Ax=b, x>=0, 
%
% whose dual is
%
% max b'y  s.t. A'y+s=p,  s>=0.
%
% calling sequence:
%
% [x,y,s,f] = pdip(A,b,p)
%
% input: A is an m x n SPARSE constraint matrix.
%        b is an m x 1 right-hand side vector
%        p is an n x 1 cost vector.
%
% output: x is the  n x 1 solution of the primal problem
%         y is the m x 1 dual solution
%         s is the n x 1 vector of "dual slacks"
%         f is the optimal objective value

if nargin ~= 3
  error('must have three input arguments');
end

if ~issparse(A)
  error('first input argument A must be a SPARSE matrix; possibly use sparse() to convert');
end

t0=cputime;
[m,n] = size(A);
if m <= 0 or n <= 0
  error('input matrix A must be nontrivial');
end

if n ~= length(p)
  error('size of vector p must match number of columns in A');
end
if m ~= length(b)
  error('size of vector b must match number of rows in A');
end

% set initial point, based on largest element in (A,b,p)
bigM = max(max(abs(A)));
bigM = max([norm(b,inf), norm(p,inf), bigM]);
x = 100*bigM*ones(n,1); s = x; y = zeros(m,1);

% find row/column ordering that gives a sparse Cholesky
% factorization of ADA'
% ordering = symmmd(A*A');
ordering = colamd(A');
bc = 1+max([norm(b), norm(p)]);

for iter=1:100
  
% compute residuals
  Rd = A'*y+s-p;
  Rp = A*x-b;
  Rc = x.*s;
  mu = mean(Rc);
  relResidual = norm([Rd;Rp;Rc])/bc;
%  fprintf('iter %2i: mu = %9.2e, resid = %9.2e\n', iter, mu, relResidual);
  fprintf('iter %2i: mu = %9.2e, resid = %9.2e\n', iter, full(mu), ...
	  full(relResidual));
  if(relResidual <= 1.e-7 & mu <= 1.e-7) break; end;
  Rc = Rc - min(0.1,100*mu)*mu;
  
  % set up the scaling matrix, and form the coefficient matrix for
  % the linear system
  d = min(5.e+15, x./s);
  B = A*sparse(1:n,1:n,d)*A';
  % use the form of the Cholesky routine "cholinc" that's best
  % suited to interior-point methods
  R = cholinc(B(ordering,ordering),'inf');
  
  % set up the right-hand side
  t1 = x.*Rd-Rc;
  t2 = -(Rp+A*(t1./s));
  
  % solve it and recover the other step components
  dy = zeros(m,1);
  dy(ordering) = R\(R'\t2(ordering));
  dx = (x.*(A'*dy)+t1)./s;
  ds = -(s.*dx+Rc)./x;
  
  tau = max(.9995,1-mu);
  ap = -1/min(min(dx./x),-1);
  ad = -1/min(min(ds./s),-1);
  ap = tau*ap;
  ad = tau*ad;
  x = x + ap*dx;
  s = s + ad*ds;
  y = y + ad*dy;
end

f = p'*x;

% convert x,y,s to full data structures
x=full(x); s=full(s); y=full(y);

fprintf('Done!\t[m n] = [%g %g]\tCPU = %g\n', m, n, cputime-t0);
return;  
