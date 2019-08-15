function [x,z,f,errorcode] = proj08(a,C,d,x,p,errorcode);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
%
% projection method in tableau form
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
%        errorcode = 4: feasible domain empty
[m,n]       = size(C);
mm          = m;
I           = 1:n;
J           = n+1:n+m-p;
IB          = [m+1:m+n-p]';
IN          = [p+1:m]';
iter        = 0;
maxiter     = 10;                       % max. step number
tol         = 1000*eps;
% -- first tableau -----------------------------------------
r           = C*x - d;
if errorcode == 0
   if (p > 0) & (any(abs(r(1:p)) > tol))
      errorcode = 1;                    % x not feasible
   end;
   r         = r(p+1:m);
   if any(r > tol)
      errorcode = 1;                    % x not feasible
   end;
end
if errorcode == 0
   C         = [C; zeros(n-p,p) eye(n-p)];
   d         = [d; x(p+1:n)];
   A         = [C(1:p,:); C(m+1:m+n-p,:)];
   A         = inv(A);
   G         = C(p+1:m,:)*A;
   w         = a'*A;
   f         = a'*x;
   P         = [A  x; G  r; w  f];
end
done = errorcode;
while ~done
   iter      = iter + 1;
   sgn       = 1;
   %-- step 1.1 -----------------------------------------------
   if any(IB > mm)
      v       = abs(w(p+1:n));
      K       = find(IB > mm);
      maxv    = max(v(K));
      j       = K(min(find(v(K) == maxv)));
      ww      = w(j+p);
      if ww > 0
         sgn = -1;
      end;
   end;
   %-- step 1.2 -----------------------------------------------
   if all(IB <= mm)
      sgn     = 1;
      v       = w(p+1:n);
      j       = min(find(v == min(v)));
      if any(r == 0)                    % Bland's rule
         k     = min(IB(find(v < 0)));
         j     = find(IB == k);
      end;
   end;
   g         = P(J,j + p);
   r         = P(J,n+1);
   if any(IB > mm) & ww == 0 & all(g >= 0)
      sgn    = - 1;
   end
   %-- step 2 -------------------------------------------------
   if all(sgn*g >= 0)
      iter
      IB
      pause
      errorcode = 2;                    % solution does not exist
   else
      h       = find(sgn*g < 0);
      s       = r(h)./(sgn*g(h));
      mins    = min(s);
      K       = find(s == mins);
      i       = h(min(K));
      if any(r == 0)                    % Bland's rule
         L     = IN(h);
         k     = min(L(K));
         i     = find(IN == k);
      end
      k       = i + n;
      %-- swapping -----------------------------------------------
      l       =   j + p;
      Q       =   P  - P(:,l)*P(k,:)/P(k,l);
      Q(:,l)  =   P(:,l)/P(k,l);
      Q(k,:)  = - P(k,:)/P(k,l);
      Q(k,l)  =   1/P(k,l);
      P       =   Q;
      %-- adaption of index vectors ------------------------------
      aux     = IB(j);
      if aux > m
         M     = 1:n+m+1-p;
         M     = M(find(M ~= k));
         P     = P(M,:);                % row i = k-n is active
         IB(j) = IN(i);
         M     = 1:m-p;
         M     = M(find(M ~= i));
         IN    = IN(M);                 % cancel i from non-basis
         m     = m - 1;
         J     = n+1:n+m-p;
      else
         IB(j) = IN(i);
         IN(i) = aux;
      end;
      w       = P(n+m+1-p,I);
   end;
   if iter > maxiter
      errorcode = 3;                    % max. step number reached
   end;
   done      = all(IB <= mm) & all(w(p+1:n) >= - tol);
   done      = done | (errorcode > 0);
   % -------------------------------------
   iter
   w
   f         = P(n+m+1-p,n+1)
   % -------------------------------------
end;

if errorcode == 0
   IB        = [[1:p]'; p+IB];
   f         = P(n+m+1-p,n+1);
   x         = P(1:n,n+1);
   x         = C(IB,:)\d(IB);          % x directly for comparison
   z         = zeros(m,1);
   z(IB)     = P(m+1,I)';
else
   f     = NaN;
   x     = NaN*ones(n,1);
   z     = NaN*ones(m,1);
end
