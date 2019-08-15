function [x,z,f,errorcode] = proj3a(a,C,d,x,p,errorcode);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% projektion method in tableau form
% problem: Max(a'*x, C*x <= d)
% C (m,n)-mMatrix, a,d,x column vectors
% start in feasible point x
% 0 <= p < n,
% first p rows of Cx <= d are equations
% ASSUMPTION:
%        (a) first p rows of C linear independent,
%        (b) rang(C) = n
% OUTPUT x extreme point of feasible domain
%        z Lagrange multiplicator
%        z(1:p) belongs to equations
%        f value of objective function
%        errorcode = 0: solution found
%        errorcode = 1: x not feasible
%        errorcode = 2: solution does not exist
%        errorcode = 3: max. step number reached
[m,n]     = size(C);
IB        = zeros(1,n-p);
IN        = 1:m-p;
I         = 1:n;
J         = n+1:n+m-p;
iter      = 0;
maxiter   = 40;                       % max. step number
tol       = 1000*eps;
% -- first tableau -----------------------------------------
A         = eye(n);
A(1:p,:)  = C(1:p,:);
A         = inv(A);
G         = C(p+1:m,:)*A;
r         = C*x - d;
w         = a'*A;
f         = a'*x;
if errorcode == 0
   if (p > 0) & (any(abs(r(1:p)) > tol))
      errorcode = 1;                    % x not feasible
   end;
   r       = r(p+1:m);
   if any(r > tol)
      errorcode = 1;                    % x not feasible
   end;
   P       = [A  x; G  r; w  f];
end
done = errorcode;
while ~done
   iter   = iter + 1
   %-- step 1.1 -----------------------------------------------
   if any(IB <= 0)
      v    = abs(w(p+1:n));
      k    = find(IB == 0);
      maxv = max(v(k));
      j    = k(min(find(v(k) == maxv)));
      if w(j+p) > 0
         g  = - P(J,j+p);                % - A(:,j) search direction
      end;
      if w(j+p) <= 0
         g  = P(J,j+p);                  %   A(:,j) search direction
      end;
      if (w(j+p) == 0) & (all(g >= 0))
         g  = - g;
      end;
      r    = P(J,n+1);
   end;
   %-- step 1.2 -----------------------------------------------
   if all(IB > 0)
      v    = w(p+1:n);
      j    = min(find(v == min(v)));
      if any(r == 0)                    % Bland's rule
         k  = min(IB(find(v < 0)));
         j  = find(IB == k);
      end;
      g    = P(J,j + p);
      r    = P(J,n+1);
   end;
   %-- step 2 -------------------------------------------------
   if all(g >= 0)
      errorcode = 2;                    % solution does not exist
   else
      h        = find(g < 0);
      s        = r(h)./g(h);
      mins     = min(s);
      L        = find(s == mins);
      i        = h(min(L));
      if any(r == 0)                    % Bland's rule
         M      = IN(h);
         i      = find(IN == min(M(L)));
      end
      k        = i + n;
      %-- exchange step ------------------------------------------
      l        =   j + p;
      Q        =   P  - P(:,l)*P(k,:)/P(k,l);
      Q(:,l)   =   P(:,l)/P(k,l);
      Q(k,:)   = - P(k,:)/P(k,l);
      Q(k,l)   =   1/P(k,l);
      P        =   Q;
      %-- adaption of index vectors ------------------------------
      ss       = IB(j);
      if ss == 0
         L      = 1:n+m+1-p;
         L      = L(find(L ~= k));
         P      = P(L,:);                % row i = k-n is active
         IB(j)  = IN(i);
         L      = 1:m-p;
         L      = L(find(L ~= i));
         IN     = IN(L);                 % cancel i from non-basis
         m      = m - 1;
         J      = n+1:n+m-p;
      else
         IB(j)  = IN(i);
         IN(i)  = ss;
      end;
      w        = P(n+m+1-p,I);
   end;
   if iter > maxiter
      errorcode = 3;                    % max. step number reached
   end;
   done = all(IB > 0) & all(w(p+1:n) >= - tol);
   done = done | (errorcode > 0);
   iter
   f    = P(n+m+1-p,n+1)
end;

if errorcode == 0
   f     = P(n+m+1-p,n+1);
   x     = P(1:n,n+1);
   z     = zeros(n+m-p,1);
   IB    = [1:p IB+ p];
   z(IB) = P(n+m+1-p,I)';
else
   f     = NaN;
   x     = NaN*ones(n,1);
   z     = NaN*ones(n+m-p,1);
end
