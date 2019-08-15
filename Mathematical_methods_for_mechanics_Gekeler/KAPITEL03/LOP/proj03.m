function [x,y,f,errorcode] = proj03(a,B,c,x,p,errorcode);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% projection method in tableau form
% problem: max(a'*x, B*x <= c, b^ix = c^i, i = 1:p)
% phase 2: start in feasible point
% B (m,n)-mMatrix, a,y row vectors, c,x column vectors
% ASSUMPTION:
%         rank(B(1:p,:)) = p
%         rank(B) = n
%         x feasible point
% OUTPUT: x solution of primal problem (if exists)
%         y solution of dual problem (if exists)
%         f value of objective function
%         errorcode = 0: solution found
%         errorcode = 1: x not feasible
%         errorcode = 2: solution does not exist
%         errorcode = 3: max. step number attained
% REMARK:
%         IB = A(x)
%         IN = N(x) complement of A(x) in [1:m]
%
maxiter = 10;                    % max. step number
tol     = 1000*eps;
[m,n] = size(B); mm = m;
IB = [1:p, m+1:m+n-p]; IN = [p+1:m]; iter = 0;
% -- first tableau ----------------------------------------
r = B*x - c;
if errorcode == 0
   if (p > 0) & (any(abs(r(1:p)) > tol))
      errorcode = 1;                    % x not feasible
   end;
   r = r(p+1:m);
   if any(r > tol), errorcode = 1; end  % x not feasible
end
if errorcode == 0
   B = [B; eye(n)]; c = [c; x];
   if p > 0
      [Q,R] = qr(B(1:p,:)');
      Q = Q(:,p+1:n)'; B = [B; Q]; c = [c; Q*x];
   end
   A  = [B(1:p,:); B(m+1:m+n-p,:)];
   A  = inv(A); D = B(p+1:m,:)*A;
   w  = a*A; f = a*x;
   P  = [A, x; D, r; w, f];
end
done = errorcode;
while ~done
   iter = iter + 1; sgn = 1;
   %-- step 1.1 ----------------------------------------------
   if any(IB > mm)
      v = abs(w(p+1:n)); K = find(IB(p+1:n) > mm);
      maxv = max(v(K));
      L  = find(v(K) == maxv); j = K(min(L)) + p;
      ww = w(j);
      if ww > 0, sgn = -1;  end
   end
   %-- step 1.2 ----------------------------------------------
   if all(IB <= mm)
      sgn = 1; v = w(p+1:n); K = find(v == min(v));
      j   = min(K) + p;
      if any(r == 0)                    % Bland's rule
         IB1 = IB(p+1:n); K = find(v < 0);
         k = min(IB1(K)); j = find(IB1 == k) + p;
      end
   end
   d = P(n+1:n+m-p,j);
   if any(IB > mm) & ww == 0 & all(d >= 0), sgn     = - 1; end
   %-- step 2 ------------------------------------------------
   d  = sgn*d;
   if all(d >= 0)
      errorcode = 2;                % solution does not exist
   else
      H = find(d < 0);
      s = r(H)./(d(H)); mins  = min(s);
      K = find(s == mins); i = H(min(K));
      if any(r == 0)                    % Bland's rule
         L = IN(H); i = find(IN == min(L(K)));
      end
      k = i + n;
      %-- swapping ----------------------------------------------
      Q  = P  - P(:,j)*P(k,:)/P(k,j);
      Q(:,j) = P(:,j)/P(k,j); Q(k,:)   = - P(k,:)/P(k,j);
      Q(k,j) = 1/P(k,j);
      P  = Q;
      %-- adaption of index vectors -----------------------------
      aux = IB(j);
      if aux > m
         K = [1:k-1, k+1:m+n+1-p];
         P = P(K,:);              % row i = k-n is active
         IB(j) = IN(i); K = [1:i-1 i+1:m-p];
         IN    = IN(K);             % cancel i from non-basis
         m     = m - 1; J = n+1:n+m-p;
      else
         IB(j) = IN(i); IN(i)  = aux;
      end
      w = P(n+m+1-p,1:n); r = P(n+1:n+m-p,n+1);
   end
   if iter > maxiter, errorcode = 3; end % max. step number reached
   done  = all(IB <= mm) & all(w(p+1:n) >= - tol);
   done  = done | (errorcode > 0);
   % -------------------------------------------------------
   iter, w, f = P(n+m+1-p,n+1)
   % -------------------------------------------------------
end
if errorcode == 0
   f = P(n+m+1-p,n+1); x = P(1:n,n+1);
   % x = B(IB,:)\c(IB)    % x directly for comparison
   y = zeros(1,m); y(IB) = P(m+1,1:n);
else
   f = NaN; x = NaN*ones(n,1); y = NaN*ones(1,m);
end
