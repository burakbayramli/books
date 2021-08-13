function [x,flag,total_iters, error]=idrs_r(aparams,mparams, b, params, x0 );

%IDRS_R Induced Dimension Reduction right preconditioning
%   Version for use in IFISS
%   input
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%          b            right-hand side vector
%          params       three-dimensional vector to control iteration
%            params(1) = relative residual reduction factor
%            params(2) = max number of iterations
%            params(3) = dimension of shadow space
%            params(4) = value of alpha to avoid omega-breakdown
%          x0           initial iterate
%   output
%          x            computed (approximate) solution
%          flag         convergence flag
%                   0 for convergence to params(1) within params(2) iterations
%                   1 if iterated params(2) times but did not converge to params(1)
%          total_iters  total iteration count
%          error        vector of residual norms of iterates

%
%   Martin van Gijzen and Peter Sonneveld
%   Copyright (c) July 2007
%
% This code comes with no guarantee or warranty of any kind.
%

%   Martin van Gijzen and Peter Sonneveld
%   Copyright (c) January 2010


% Names of mvp and preconditioning routines
afun = aparams.Afun;
mfun = mparams.Mfun;

%
% initialization
%
n=length(b);
errtol=params(1);
kmax=params(2);
s = params(3);
angle = params(4);
x=zeros(n,1);
flag=0;

if norm(x0) ~=0
   r = b-feval(afun,x0,aparams);
   x = x0;
else
   r = b;
end

randn('state', 0);
Q = randn(n,s);
Q = orth(Q);

errtol=errtol*norm(b);
error=[];
normr = norm(r);
%
% test for termination on entry
%
error=[error,normr];
total_iters=0;
if (normr < errtol)
   return
end

G = zeros(n,s); U = zeros(n,s); 
M = eye(s,s); 
om = 1;

% Main iteration loop, build G-spaces:
iter = 0;
while ( normr > errtol & iter < kmax )  

% New righ-hand size for small system:
   f = (r'*Q)';
   for k = 1:s 

% Solve small system and make v orthogonal to Q:
      c = M(k:s,k:s)\f(k:s); 
      v = r - G(:,k:s)*c;
      v = feval(mfun,v,aparams,mparams);

      U(:,k) = U(:,k:s)*c + om*v;
% Compute G(:,k) = A U(:,k) 
      G(:,k) = feval(afun, U(:,k), aparams);
%
% Bi-Orthogonalise the new basis vectors: 
      for i = 1:k-1
         alpha =  ( Q(:,i)'*G(:,k) )/M(i,i);
         G(:,k) = G(:,k) - alpha*G(:,i);
         U(:,k) = U(:,k) - alpha*U(:,i);
      end
% New column of M = Q'*G  (first k-1 entries are zero)
      M(k:s,k) = (G(:,k)'*Q(:,k:s))';
%
%  Make r orthogonal to p_i, i = 1..k 
      gamma = f(k)/M(k,k);
      r = r - gamma*G(:,k);
      x = x + gamma*U(:,k);

      iter = iter + 1;
      normr = norm(r);
      error = [error;normr];
      if ( normr < errtol | iter == kmax ) 
         if normr<errtol, flag=0; else flag=1; end
         error = error';
         total_iters = iter;
         return;
      end 

% New f = Q'*r (first k  components are zero)
      if ( k <s ) 
         f(k+1:s)   = f(k+1:s) - gamma*M(k+1:s,k);
      end
   end 

% Now we have sufficient vectors in G_j to compute residual in G_j+1
% Note: r is already perpendicular to Q so v = r
   v = feval(mfun,r,aparams,mparams);
   t = feval(afun, v, aparams);
   om = omega( t, r, angle );
%
   r = r - om*t;
   x = x + om*v;
   iter = iter + 1;

   normr = norm(r);
   error = [error;normr];

end; %while

if normr<errtol, flag=0; else flag=1; end

error = error';
total_iters = iter;

return

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function om = omega( t, s, angle )

ns = norm(s);
nt = norm(t);
ts = dot(t,s);
rho = abs(ts/(nt*ns));
om=ts/(nt*nt);
if ( abs(rho ) < angle )
   om = om*angle/abs(rho);
end

return
