function [x,y,s,f] = pathfollow(A,b,p)
% syntax: [x,y,s,f] = pdip(A,b,p)
%
% path-following primal-dual interior-point method for problem 
%
% PRIMAL: min p'x s.t. Ax=b, x>=0, 
% DUAL:   max b'y  s.t. A'y+s=p,  s>=0.
%
% input: A is an m x n SPARSE constraint matrix.
%        b is an m x 1 right-hand side vector
%        p is an n x 1 cost vector.
%
% output: x is the  n x 1 solution of the primal problem
%         y is the m x 1 dual solution
%         s is the n x 1 vector of "dual slacks"
%         f is the optimal objective value
%
% internal parameters: 
%         itmax is the maximum number of iterations allowed
%         tol is the convergence tolerance
%         bigMfac is the factor used to define the starting point
%         maxDiag is the element for the X^{-1}S matrix
%         etaMin is the minimum value of the steplength scale parameter eta

% check validity of input arguments
if nargin ~= 3
  error('must have three input arguments');
end

if ~issparse(A)
  error('first input argument A must be a SPARSE matrix; possibly use sparse() to convert');
end

[m,n] = size(A);

% set the internal parameters
itmax = 100; 
tol = 1.e-7;
bigMfac = 100;
maxDiag = 5.e+15;
etaMin = .9995;

% start the clock
t0=cputime;

% set initial point, based on largest element in (A,b,p)
bigM = max(max(abs(A)));
bigM = max([norm(b,inf), norm(p,inf), bigM]);
x = bigMfac*bigM*ones(n,1); s = x; y = zeros(m,1);

% find row/column ordering for a sparse Cholesky factorization of ADA'
% ordering = symamd(A*A');
ordering = colamd(A');
bc = 1+max([norm(b), norm(p)]);

for iter=1:itmax
  
  % compute residuals
  Rd = A'*y+s-p;
  Rp = A*x-b;
  Rc = x.*s;
  mu = mean(Rc);

  % check relative decrease in residual, for purposes of convergence test
  relResidual = norm([Rd;Rp;Rc])/bc;
  fprintf(1,'iter %2i: mu = %9.2e, resid = %9.2e\n', iter, full(mu), ...
	  full(relResidual));

  % test for convergence
  if(relResidual <= tol && mu <= tol) break; end;

  % make a heuristic choice of the centering parameter, and adjust the 
  % right-hand side
  sigma = min(0.1,100*mu);
  Rc = Rc - sigma*mu;
  
  % set up the scaling matrix and form the coef matrix for normal equations
  d = min(maxDiag, x./s);
  B = A*sparse(1:n,1:n,d)*A';
  % use the form of the Cholesky routine "cholinc" that's best
  % suited to interior-point methods
  %R = cholinc(B(ordering,ordering),'inf');
  R = chol(B(ordering,ordering));
  #R = ichol(B(ordering,ordering),'inf');
  
  % set up the right-hand side
  t1 = x.*Rd-Rc;
  t2 = -(Rp+A*(t1./s));
  
  % solve the normal equations system for dy and recover the other 
  %  step components dx and ds
  dy = zeros(m,1);
  dy(ordering) = R\(R'\t2(ordering));
  dx = (x.*(A'*dy)+t1)./s;
  ds = -(s.*dx+Rc)./x;
 
  % set the parameter eta defining fraction of max step to boundary
  eta = max(etaMin,1-mu);
  [alpha, alphax, alphas] = steplength(x, s, dx, ds, eta);

  % take the step
  x = x + alphax * dx;
  s = s + alphas * ds;
  y = y + alphas * dy;
end

% calculate final primal objective value
f = p'*x;

% convert x,y,s to full data structures
x=full(x); s=full(s); y=full(y);

fprintf('Done!\t[m n] = [%g %g]\tCPU = %g\n', m, n, cputime-t0);
return;  
