function [x,x_best,flag,relres,iter,resvec,lsvec] = lsqr(A,b,tol,maxit,M1,M2,x0,varargin)
%LSQR   LSQR Method.
%   X = LSQR(A,B) attempts to solve the system of linear equations A*X=B
%   for X if A is consistent, otherwise it attempts to solve the least
%   squares solution X that minimizes norm(B-A*X). The N-by-P coefficient
%   matrix A need not be square but the right hand side column vector B
%   must have length N.
%
%   X = LSQR(AFUN,B) accepts a function handle AFUN instead of the matrix A.
%   AFUN(X,'notransp') accepts a vector input X and returns the
%   matrix-vector product A*X while AFUN(X,'transp') returns A'*X. In all
%   of the following syntaxes, you can replace A by AFUN.
%
%   X = LSQR(A,B,TOL) specifies the tolerance of the method. If TOL is []
%   then LSQR uses the default, 1e-6.
%
%   X = LSQR(A,B,TOL,MAXIT) specifies the maximum number of iterations. If
%   MAXIT is [] then LSQR uses the default, min([N,P,20]).
%
%   X = LSQR(A,B,TOL,MAXIT,M) and LSQR(A,B,TOL,MAXIT,M1,M2) use P-by-P
%   preconditioner M or M = M1*M2 and effectively solve the system
%   A*inv(M)*Y = B for Y, where Y = M*X. If M is [] then a preconditioner
%   is not applied. M may be a function handle MFUN such that
%   MFUN(X,'notransp') returns M\X and MFUN(X,'transp') returns M'\X.
%
%   X = LSQR(A,B,TOL,MAXIT,M1,M2,X0) specifies the P-by-1 initial guess. If
%   X0 is [] then LSQR uses the default, an all zero vector.
%
%   [X,FLAG] = LSQR(A,B,...) also returns a convergence FLAG:
%    0 LSQR converged to the desired tolerance TOL within MAXIT iterations.
%    1 LSQR iterated MAXIT times but did not converge.
%    2 preconditioner M was ill-conditioned.
%    3 LSQR stagnated (two consecutive iterates were the same).
%    4 one of the scalar quantities calculated during LSQR became too
%      small or too large to continue computing.
%
%   [X,FLAG,RELRES] = LSQR(A,B,...) also returns estimates of the relative
%   residual NORM(B-A*X)/NORM(B). If RELRES <= TOL, then X is a
%   consistent solution to A*X=B. If FLAG is 0 but RELRES > TOL, then X is
%   the least squares solution which minimizes norm(B-A*X).
%
%   [X,FLAG,RELRES,ITER] = LSQR(A,B,...) also returns the iteration number
%   at which X was computed: 0 <= ITER <= MAXIT.
%
%   [X,FLAG,RELRES,ITER,RESVEC] = LSQR(A,B,...) also returns a vector of
%   estimates of the residual norm at each iteration including NORM(B-A*X0).
%
%   [X,FLAG,RELRES,ITER,RESVEC,LSVEC] = LSQR(A,B,...) also returns a vector
%   of least squares estimates at each iteration:
%   NORM((A*inv(M))'*(B-A*X))/NORM(A*inv(M),'fro'). Note the estimate of
%   NORM(A*inv(M),'fro') changes, and hopefully improves, at each iteration.
%
%   Example:
%      n = 100; on = ones(n,1); A = spdiags([-2*on 4*on -on],-1:1,n,n);
%      b = sum(A,2); tol = 1e-8; maxit = 15;
%      M1 = spdiags([on/(-2) on],-1:0,n,n);
%      M2 = spdiags([4*on -on],0:1,n,n);
%      x = lsqr(A,b,tol,maxit,M1,M2);
%   Or, use this matrix-vector product function
%      %-----------------------------------%
%      function y = afun(x,n,transp_flag)
%      if strcmp(transp_flag,'transp')
%         y = 4 * x;
%         y(1:n-1) = y(1:n-1) - 2 * x(2:n);
%         y(2:n) = y(2:n) - x(1:n-1);
%      elseif strcmp(transp_flag,'notransp')
%         y = 4 * x;
%         y(2:n) = y(2:n) - 2 * x(1:n-1);
%         y(1:n-1) = y(1:n-1) - x(2:n);
%      end
%      %-----------------------------------%
%   as input to LSQR:
%      x1 = lsqr(@(x,tflag)afun(x,n,tflag),b,tol,maxit,M1,M2);
%
%   Class support for inputs A,B,M1,M2,X0 and the output of AFUN:
%      float: double
%
%   See also BICG, BICGSTAB, CGS, GMRES, MINRES, PCG, QMR, SYMMLQ, LUINC,
%   FUNCTION_HANDLE.

%   Copyright 1984-2005 The MathWorks, Inc.
%   $Revision: 1.6.4.5 $ $Date: 2005/12/12 23:26:36 $

% Check for an acceptable number of input arguments
if nargin < 2
   error('MATLAB:lsqr:NotEnoughInputs', 'Not enough input arguments.');
end
% If A is a function, then it could take awhile to discover the value N
nKnown = false;
% Determine whether A is a matrix or a function.
[atype,afun,afcnstr] = iterchk(A);
if strcmp(atype,'matrix')
   % Check matrix and right hand side vector inputs have appropriate sizes
   [m,n] = size(A);
   nKnown = true;
   if ~isequal(size(b),[m,1])
      error('MATLAB:lsqr:RSHsizeMatchCoeffMatrix', ...
         ['Right hand side must be a column vector of' ...
         ' length %d to match the coefficient matrix.'],m);
   end
else
   m = size(b,1);
   if ~isvector(b) || (size(b,2) ~= 1) % if ~isvector(b,'column')
      error('MATLAB:lsqr:RSHnotColumn', 'Right hand side must be a column vector.');
   end
end

% Assign default values to unspecified parameters
if nargin < 3 || isempty(tol)
   tol = 1e-6;
end

if nargin < 4 || isempty(maxit)
   maxit = min([m,n,20]);
end

if ((nargin >= 5) && ~isempty(M1))
   existM1 = 1;
   [m1type,m1fun,m1fcnstr] = iterchk(M1);
   if strcmp(m1type,'matrix')
      if nKnown
         if ~isequal(size(M1),[n,n])
            error('MATLAB:lsqr:WrongPrecondSize', ...
               ['Preconditioner must be a square matrix' ...
               ' of size %d to match the problem size.'],n);
         end
      else
         n = size(M1,1);
         if (size(M1,2) ~= n)
            error('MATLAB:lsqr:WrongPrecondSquareSize', ...
               'Preconditioner must be a square matrix.');
         end
         nKnown = true;
      end
   end
else
   existM1 = 0;
   m1type = 'matrix';
end

if ((nargin >= 6) && ~isempty(M2))
   existM2 = 1;
   [m2type,m2fun,m2fcnstr] = iterchk(M2);
   if strcmp(m2type,'matrix')
      if nKnown
         if ~isequal(size(M2),[n,n])
            error('MATLAB:lsqr:WrongPrecondSize', ...
               ['Preconditioner must be a square matrix' ...
               ' of size %d to match the problem size.'],n);
         end
      else
         n = size(M2,1);
         if (size(M2,2) ~= n)
            error('MATLAB:lsqr:WrongPrecondSquareSize', ...
               'Preconditioner must be a square matrix.');
         end
         nKnown = true;
      end
   end
else
   existM2 = 0;
   m2type = 'matrix';
end

xInit = false;
if ((nargin >= 7) && ~isempty(x0))
   if nKnown
      if ~isequal(size(x0),[n,1])
         error('MATLAB:lsqr:WrongInitGuessSize', ...
            ['Initial guess must be a column vector of' ...
            ' length %d to match the problem size.'],n);
      else
         n = size(x0,1);
         nKnown = true;
         if ~isvector(x0) || (size(x0,2) ~= 1) % if ~isvector(x0,'column')
            error('MATLAB:lsqr:WrongInitGuessVector', ...
               'Initial guess must be a column vector.');
         end
         x = x0;
         xInit = true;
      end
   end
end

if ((nargin > 7) && strcmp(atype,'matrix') && ...
      strcmp(m1type,'matrix') && strcmp(m2type,'matrix'))
   error('MATLAB:lsqr:TooManyInputs','Too many input arguments.');
end

if ~nKnown
   % How many columns does A have?  Same as entries of x = A'*b.
   x = iterapp('mtimes',afun,atype,afcnstr,b,varargin{:},'transp');
   n = size(x,1);
end

if ~xInit
   x = zeros(n,1);
end

% Set up for the method
n2b = norm(b);                     % Norm of rhs vector, b
flag = 1;
tolb = tol * n2b;                  % Relative tolerance
u = b - iterapp('mtimes',afun,atype,afcnstr,x,varargin{:},'notransp');
beta = norm(u);
% Norm of residual r=b-A*x is estimated well by prod_i abs(sin_i)
normr = beta;
if beta ~= 0
   u = u / beta;
end
c = 1;
s = 0;
phibar = beta;
d = zeros(n,1);
v = iterapp('mtimes',afun,atype,afcnstr,u,varargin{:},'transp');
if existM1
   v = iterapp('mldivide',m1fun,m1type,m1fcnstr,v,varargin{:},'transp');
   if any(~isfinite(v))
      flag = 2;
      relres = normr/n2b;
      iter = 0;
      resvec = normr;
      lsvec = zeros(0,1);
      if nargout < 2
         itermsg('lsqr',tol,maxit,0,flag,iter,relres);
      end
      return
   end
end
if existM2
   v = iterapp('mldivide',m2fun,m2type,m2fcnstr,v,varargin{:},'transp');
   if any(~isfinite(v))
      flag = 2;
      relres = normr/n2b;
      iter = 0;
      resvec = normr;
      lsvec = zeros(0,1);
      if nargout < 2
         itermsg('lsqr',tol,maxit,0,flag,iter,relres);
      end
      return
   end
end
alpha = norm(v);
if alpha ~= 0
   v = v / alpha;
end

% norm((A*inv(M))'*r) = alpha_i * abs(sin_i * phi_i)
normar = alpha * beta;

% Check for all zero solution
if (normar == 0)                     % if alpha_1 == 0 | beta_1 == 0
   x = zeros(n,1);                  % then  solution is all zeros
   flag = 0;                        % a valid solution has been obtained
   relres = 0;                      % the relative residual is actually 0/0
   iter = 0;                        % no iterations need be performed
   resvec = beta;                   % resvec(1) = norm(b-A*x) = norm(0)
   lsvec = zeros(0,1);              % no estimate for norm(A*inv(M),'fro') yet
   if (nargout < 2)
      itermsg('lsqr',tol,maxit,0,flag,iter,NaN);
   end
   return
end

% Poorly estimate norm(A*inv(M),'fro') by norm(B_{i+1,i},'fro')
% which is in turn estimated very well by sqrt(sum_i (alpha_i^2 + beta_{i+1}^2))
norma = 0;
% norm(inv(A*inv(M)),'fro') = norm(D,'fro')
% which is poorly estimated by sqrt(sum_i norm(d_i)^2)
sumnormd2 = 0;
resvec = zeros(maxit+1,1);         % Preallocate vector for norm of residuals
resvec(1) = normr;                 % resvec(1,1) = norm(b-A*x0)
lsvec = zeros(maxit,1);            % Preallocate vector for least squares estimates
stag = 0;                          % stagnation of the method
iter = maxit;                      % Assume lack of convergence until it happens

% loop over maxit iterations (unless convergence or failure)

for i = 1 : maxit
   if existM2
      z = iterapp('mldivide',m2fun,m2type,m2fcnstr,v,varargin{:},'notransp');
      if any(~isfinite(z))
         flag = 2;
         iter = i-1;
         resvec = resvec(1:iter+1);
         lsvec = lsvec(1:iter);
         break
      end
   else
      z = v;
   end
   if existM1
      z = iterapp('mldivide',m1fun,m1type,m1fcnstr,z,varargin{:},'notransp');
      if any(~isfinite(z))
         flag = 2;
         iter = i-1;
         resvec = resvec(1:iter+1);
         lsvec = lsvec(1:iter);
         break
      end
   end
   u = iterapp('mtimes',afun,atype,afcnstr,z,varargin{:},'notransp') - alpha * u;
   beta = norm(u);
   u = u / beta;
   norma = norm([norma alpha beta]);
   lsvec(i) = normar / norma;
   thet = - s * alpha;
   rhot = c * alpha;
   rho = sqrt(rhot^2 + beta^2);
   c = rhot / rho;
   s = - beta / rho;
   phi = c * phibar;
   if (phi == 0)                  % stagnation of the method
      stag = 1;
   end
   phibar = s * phibar;
   d = (z - thet * d) / rho;
   sumnormd2 = sumnormd2 + (norm(d))^2;
%    conda = norma * sqrt(sumnormd2);

   % Check for stagnation of the method
   if (stag == 0)
      stagtest = zeros(n,1);
      ind = (x ~= 0);
      stagtest(ind) = d(ind) ./ x(ind);
      stagtest(~ind & d ~= 0) = Inf;
      if norm(stagtest,inf) < eps
         stag = 1;
      end
   end

   if normar/(norma*normr) <= tol % check for convergence in min{|b-A*x|}
      flag = 0;
      iter = i-1;
      resvec = resvec(1:iter+1);
      lsvec = lsvec(1:iter+1);
      break
   end

   if normr <= tolb               % check for convergence in A*x=b
      flag = 0;
      iter = i-1;
      resvec = resvec(1:iter+1);
      lsvec = lsvec(1:iter+1);
      break
   end

   if stag == 1
      flag = 3;
      iter = i-1;
      resvec = resvec(1:iter+1);
      lsvec = lsvec(1:iter+1);
      break
   end
   
   % My Code
   % Regularization by Generalized Cross Validation
  
   GCV(i) = (resvec(i)/(length(x)-i))^2;
   if i == 1
       x_best = x; GCV_best = GCV(i);
   else
       if GCV(i) < GCV_best
           GCV_best = GCV(i);
           x_best = x;
       end
   end
   
   % Ends Here

   x = x + phi * d;
   normr = abs(s) * normr;
   resvec(i+1) = normr;
   vt = iterapp('mtimes',afun,atype,afcnstr,u,varargin{:},'transp');
   if existM1
      vt = iterapp('mldivide',m1fun,m1type,m1fcnstr,vt,varargin{:},'transp');
      if any(~isfinite(vt))
         flag = 2;
         iter = i;
         resvec = resvec(1:iter+1);
         lsvec = lsvec(1:iter+1);
         break
      end
   end
   if existM2
      vt = iterapp('mldivide',m2fun,m2type,m2fcnstr,vt,varargin{:},'transp');
      if any(~isfinite(vt))
         flag = 2;
         iter = i;
         resvec = resvec(1:iter+1);
         lsvec = lsvec(1:iter);
         break
      end
   end
   v = vt - beta * v;
   alpha = norm(v);
   v = v / alpha;
   normar = alpha * abs( s * phi);

end                                % for i = 1 : maxit

relres = normr/n2b;

% only display a message if the output flag is not used
if nargout < 2
   itermsg('lsqr',tol,maxit,i,flag,iter,relres);
end
