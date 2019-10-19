function x = simplex(A,b,p,lb,ub)
% syntax: x = simplex(A,b,p,lb,ub)
% A two-phase simplex routine for min p'x st Ax=b, lb<=x<=ub.

if nargin < 3, error('require three input arguments'); end;
if nargin < 5, ub = inf*ones(size(p));
  if nargin < 4, lb = zeros(size(p)); end; end;
[m,n] = size(A); zer_tol = 1.0e-5; piv_tol = 1.0e-8;
 
% set up Phase I problem
A = sparse(A); v = zeros(n,1);
lbdd = find(lb>-Inf); v(lbdd) = lb(lbdd); free = setdiff(1:n,lbdd);
ubdd = free(find(ub(free)<Inf)); v(ubdd) = ub(ubdd); 
if isempty(ubdd), N = -lbdd'; 
else
  N = [-lbdd' ubdd]; free = setdiff(free,ubdd);
end;
f = length(free);
if f>0
  [L,U,P,Q] = lu(A(:,free));
  free = free*Q';
  i = length(find(abs(diag(U))>piv_tol)); j = free(i+1:f);
  if ~isempty(j)
    N = [N j]; free = setdiff(free,j); f = length(free);
    lb(j) = zeros(size(j)); ub(j) = zeros(size(j));
  end;
  [k,i] = find(P(1:f,:)); % relies on diag(U) having zeros at end
  v(free) = U(1:f,1:f)\(L(1:f,1:f)\(P(1:f,:)*(b - A*v)));
  k = setdiff(1:m,k); m = m - f; B = [free n+1:n+m];
  A = [A sparse(k,1:m,sign(b(k)-A(k,:)*v+eps),m+f,m)];
else
  B = n+1:n+m; j = []; A = [A sparse(1:m,1:m,sign(b-A*v+eps*ones(size(b))))];
end;
lb = [lb; zeros(m,1)]; ub = [ub; Inf*ones(m,1)]; w = [zeros(n,1); ones(m,1)]; 
[x,B,N] = rsmbdd(A,b,w,lb,ub,B,N);
if (w'*x > zer_tol), error('problem is infeasible'); end;

% convert to Phase II problem
ub(n+1:n+m) = zeros(m,1); p = [p; zeros(m,1)];
if ~isempty(j) 
  c = p(j)'-p(B)'*inv(A(:,B))*A(:,j); % should use LU decomp from rsmbdd
  if (norm(c,inf) > zer_tol), error('problem is unbounded'); end;
end;

[x,B,N] = rsmbdd(A,b,p,lb,ub,B,N); x = x(1:n); return;

