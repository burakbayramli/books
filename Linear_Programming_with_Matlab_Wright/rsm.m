function [x_B,B,u] = rsm(A,b,p,B)
% syntax: [x_B,B,u] = rsm(A,b,p,B)
% A revised simplex routine for min p'x st Ax=b, x>=0.
% on input A is mxl, b is mx1, p is lx1 
% B is 1xm index vector denoting the basic columns.
% on output u is the dual solution

[m,l] = size(A);
zer_tol = 1.0e-5; piv_tol = 1.0e-8;
N = setdiff(1:l,B);

while (1)
  [L,U] = lu(A(:,B));
  x_B = U\(L\b);
  if any(x_B < -zer_tol) 
    error('current point is infeasible'); end;
  
  u = L'\(U'\p(B));
  c = p(N)'-u'*A(:,N); 

  if isempty(find(c < -zer_tol))
    return; end;
  
  [min_red_cost,s] = min(c);

  d = U\(L\A(:,N(s)));
  blocking = find(d >= piv_tol);
  if isempty(blocking)
    error('problem is unbounded'); end;
  
  [min_ratio,index_r] = min(x_B(blocking)./d(blocking));
  r = blocking(index_r);
  
  swap = B(r); B(r) = N(s); N(s) = swap;
end;

