function [x,B,N] = rsmbdd(A,b,p,lb,ub,B,N)
%syntax: [x,B,N] = rsmbdd(A,b,p,lb,ub,B,N)
% A revised simplex routine for min p'x st Ax=b, lb<=x<=ub.
% B is 1xm index vector denoting the basic columns.
% N is 1x(l-m) index vector denoting the nonbasic columns and bounds (+/-1).

[m,l] = size(A);
if (size(B,2) ~= m) error('B must be a row vector, size m'); end;
if (size(N,2) ~= l-m) error('N must be a row vector, size l-m'); end;
zer_tol = 1.0e-5; piv_tol = 1.0e-8;
[L,U] = lu(A(:,B));
if ~isempty(find(abs(diag(U))<piv_tol))
  error('initial basis is not invertible'); end
x = lb;  upper = N(find(N>0)); x(upper) = ub(upper);
x(B) = U\(L\(b-A(:,abs(N))*x(abs(N))));
while (1)
  if any(x(B) < lb(B)-zer_tol | x(B) > ub(B)+zer_tol) 
    error('current point is infeasible'); end;
  u = L'\(U'\p(B)); c = p(abs(N))'-u'*A(:,abs(N));
  if isempty(find(c.*N>zer_tol))
    return; end;
  [min_red_cost,s] = max(c.*sign(N));
  blk = abs(N(s)); min_ratio = ub(blk) - lb(blk); order = -sign(N(s));
  d = order*U\(L\A(:,blk));
  block_lb = find(d >= piv_tol & lb(B) > -1e20);
  if ~isempty(block_lb)
    [min_lb,index_r] = min((x(B(block_lb))-lb(B(block_lb)))./d(block_lb));
    if (min_lb < min_ratio) 
      r = block_lb(index_r); min_ratio = min_lb; blk = B(r); end; end
  block_ub = find(d <= -piv_tol & ub(B) < 1e20);
  if ~isempty(block_ub)
    [min_ub,index_r] = min((x(B(block_ub))-ub(B(block_ub)))./d(block_ub));
    if (min_ub < min_ratio) 
      r = block_ub(index_r); min_ratio = min_ub; blk = B(r); end; end
  if min_ratio > 1e20 error('problem is unbounded'); end;
  x(B) = x(B) - min_ratio*d;
  x(abs(N(s))) = x(abs(N(s))) + order*min_ratio;
  if (blk == abs(N(s))) N(s) = -N(s); else
    swap = B(r); B(r) = abs(N(s)); N(s) = -sign(d(r))*swap;
    [L,U] = lu(A(:,B)); end
end;

