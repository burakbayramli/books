function [x,y,f,errorcode] = proj02(a,B,c,x)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% projection method in tableau form
% problem max(a'*x, B*x <= c)
% phase2: start in feasible point x
% B (m,n)-matrix, a,y row vectors, c,x column vectors
% ASSUMPTION:
%        rank(B) = n
%        x feasible for (P)
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
maxiter = 20;                   % maximum step number
tol     = 1000*eps              % tolerance
[m,n] = size(B), IB = zeros(1,n); IN = 1:m;
iter  = 0; errorcode = 0;
% -- first tableau ----------------------------------------
A = eye(n); D = B*A; r = B*x - c; w = a*A; f = a*x;
P = [A, x;D, r;w, f];
if any(r > 0), errorcode  = 1; end        % x not feasible
iter = 0; done = errorcode;
while ~done
   iter = iter + 1
   %-- step 1.1 ----------------------------------------------
   if any(IB == 0)
      v    = abs(w);    K = find(IB == 0);
      maxv = max(v(K)); L = find(v(K) == maxv); j = K(min(L));
      if w(j) > 0, d = - P(n+1:n+m,j); end  % - A(:,j) search direction
      if w(j) <= 0,d = P(n+1:n+m,j);   end  %   A(:,j) search direction
      if (w(j) == 0) & (all(d >= 0)), d = - d; end
   end
   %-- step 1.2 -----------------------------------------------
   if all(IB > 0)
      K  = find(w == min(w)); j = min(K);
      if any(r == 0)                    % Bland's rule
         K = find(w < 0); k = min(IB(K)); j = find(IB == k);
      end;
      d = P(n+1:n+m,j);
   end
   %-- step 2 -------------------------------------------------
   if all(d >= 0)
      errorcode = 2;                 % solution does not exist
   else
      H = find(d < 0); s = r(H)./d(H); mins = min(s);
      K = find(s == mins); i = H(min(K));
      if any(r == 0)                    % Bland's rule
         L = IN(H); i = find(IN == min(L(K)));
      end
      %-- exchange step -----------------------------------------
      k = i + n;
      Q = P - P(:,j)*P(k,:)/P(k,j);
      Q(:,j) = P(:,j)/P(k,j); Q(k,:) = - P(k,:)/P(k,j);
      Q(k,j) = 1/P(k,j);
      P = Q;
      %-- adaption of index vectors -----------------------------
      aux = IB(j);
      if aux == 0
         L = 1:n+m+1; L = L(find(L ~= k));
         P = P(L,:);               % row i = k - n active
         IB(j)  = IN(i);
         L  = 1:m; L = L(find(L ~= i));
         IN = IN(L);            % cancel i from non-basis
         m  = m - 1; J = n+1:n+m;
      end
      if aux ~= 0, IB(j) = IN(i); IN(i) = aux; end
      w = P(n+m+1,1:n); r = P(n+1:n+m,n+1);
   end
   if iter > maxiter, errorcode = 3; end  % max. step number reached
   done = (all(IB > 0)) & (all(w >= - tol));
   done = done | (errorcode > 0);
   iter, IB
end
if errorcode == 0
   f = P(n+m+1,n+1); x     = P(1:n,n+1);
   y = zeros(1,n+m); y(IB) = P(n+m+1,1:n);
else
   f = NaN*P(n+m+1,n+1); x = NaN*P(1:n,n+1);
   y = NaN*zeros(1,n+m);
end
