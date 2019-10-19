function x = simpbdd(A,b,p,l,u)
% syntax: x = simpbdd(A,b,p,l,u)
% A two-phase simplex routine for min p'x st Ax=b, l<=x<=u.
% on input A is mxn, b is mx1, p, l, u are nx1 

if nargin < 3
  error('require three input arguments');
end
if nargin < 5
  u = inf*ones(size(p));
  if nargin < 4
    l = zeros(size(p));
  end
end
[m,n] = size(A); zer_tol = 1.0e-5; piv_tol = 1.0e-8;
 
% set up Phase I problem
v = zeros(n,1);
lbdd = find(l>-Inf); v(lbdd) = l(lbdd); N = -lbdd';
free = setdiff(1:n,lbdd);
ubdd = free(find(u(free)<Inf)); v(ubdd) = u(ubdd); 
N = [N ubdd];
free = setdiff(free,ubdd); f = length(free);
if f>0
  [L,U,P] = lu(A(:,free));
  j = free([find(abs(diag(U))<piv_tol)' m+1:f]);
  if ~isempty(j)
    N = [N j];
    free = setdiff(free,j); f = length(free);
    l(j) = zeros(size(j)); u(j) = zeros(size(j));
  end
  % following relies on diag(U) having zeros at end
  [k,i] = find(P(1:f,:));
  v(free) = U(1:f,1:f)\(L(i,1:f)\(P(i,:)*(b - A*v)));
  k = setdiff(1:m,i);
  m = m - f;
  B = [free n+1:n+m];
  A = [A sparse(k,1:m,sign(b(k)-A(k,:)*v+eps),m+f,m)];
else
  j = [];
  B = n+1:n+m;
  A = [A sparse(1:m,1:m,sign(b-A*v))];
end
l = [l; zeros(m,1)]; 
u = [u; Inf*ones(m,1)]; 
w = [zeros(n,1); ones(m,1)]; 
[x,B,N] = rsmbdd(A,b,w,l,u,B,N);
if (w'*x > zer_tol)
  error('problem is infeasible'); end;

% convert to Phase II problem
p = [p; zeros(m,1)];
if ~isempty(j)
  % could use LU decomp from rsmbdd here
  c = p(j)'-p(B)'*inv(A(:,B))*A(:,j);
  if (norm(c,inf) > zer_tol)
    error('problem is unbounded'); 
  end;
end;
u(n+1:n+m) = zeros(m,1);

[x,B,N] = rsmbdd(A,b,p,l,u,B,N);
x = x(1:n);

return;
