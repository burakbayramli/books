function [x_B,B] = rsmupd(A,b,p,B)
% syntax: [x_B,B] = rsmupd(A,b,p,B)
% A revised simplex routine for min p'x st Ax=b, x>=0.
% on input A is mxn, b is mx1, p is nx1 
% B is 1xm index vector denoting the basic columns.

[m,n] = size(A);
zer_tol = 1.0e-5; piv_tol = 1.0e-8; slack_tol = 1.0e-6;
N = setdiff(1:n,B); 
max_upd = 10; H = zeros(m,max_upd); S = zeros(max_upd,1); num_upd = 0;

perm = colamd(A(:,B)); B = B(perm);
[L,U,P] = lu(A(:,B));
x_B = U\(L\(P*b));

%iter = 0;
while (1)
%  iter = iter + 1;
%  fprintf('iter = %d, num_upd = %d\n',iter,num_upd);
  if any(x_B < -zer_tol) 
    error('current point is infeasible'); end;
  
  if num_upd > 0
    [L1,U1] = lu(eye(num_upd) + H(S(1:num_upd),1:num_upd));
    e = H(:,1:num_upd)'*p(B);
    d = zeros(m,1); d(S(1:num_upd)) = L1'\(U1'\e); d = p(B) - d;
  else
    d = p(B);
  end
  u = P'*(L'\(U'\d));
  c = p(N)'-u'*A(:,N); 

  if isempty(find(c < -zer_tol))
    return; end;
  
  [min_red_cost,s] = min(c);

  d = U\(L\(P*A(:,N(s))));
  if num_upd > 0
    e = U1\(L1\d(S(1:num_upd)));
    d = d - H(:,1:num_upd)*e;
  end

  blocking = find(d >= piv_tol);
  if isempty(blocking)
    error('problem is unbounded'); end;
  
  elbow = x_B(blocking)+slack_tol;
  min_ratio = min(elbow./d(blocking));

  eligible = find(x_B(blocking)./d(blocking) <= min_ratio);
  [max_piv,index_r] = max(d(blocking(eligible)));
  r = blocking(eligible(index_r));
  min_ratio = x_B(r)/d(r);
  
  swap = B(r); B(r) = N(s); N(s) = swap;
  x_B = x_B - min_ratio*d; 
  x_B(r) = min_ratio;

  if num_upd < max_upd 
    if isempty(find(S(1:num_upd)==r))
      num_upd = num_upd + 1;
      S(num_upd) = r;
      H(:,num_upd) = U\(L\(P*(A(:,B(r))-A(:,N(s)))));
    else
      i = find(S(1:num_upd)==r);
      H(:,i) = H(:,i) + U\(L\(P*(A(:,B(r))-A(:,N(s)))));
    end
  else
    num_upd = 0;
    perm = colamd(A(:,B)); B = B(perm); x_B = x_B(perm);
    [L,U,P] = lu(A(:,B));
%    if any((A(:,B)*x_B-b) > slack_tol)
%      x_B = U\(L\(P*b));
%    end
  end
end;

