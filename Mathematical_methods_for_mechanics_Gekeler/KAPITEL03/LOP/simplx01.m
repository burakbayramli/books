function [x,y,f,IB,errorcode] = simplx01(a,B,c,y,IB,errorcode);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% simplex method in tableau form, row problem
% problem: min(y*c, y*B = a, y >= 0)
% phase 2: start in vertex y
% B (m,n)-Matrix, a,y row vectors, c,x column vectors
% ASSUMPTION:
%         rang(B) = n
%         y vertex with row basis IB
% OUTPUT:
%         x solution of primal problem (if exists)
%         y solution of dual problem (simplex problem)
%         f value of objective function
%         errorcode = 0: solution found
%         errorcode = 1: y not feasible
%         errorcode = 2: solution does not exist
%         errorcode = 3: max. step number reached
% REMARK:
%         IB := A(y)
%         IN := N(y) complement of IB in [1:m]
%         (y(i) = 0, i in IN)
%
maxiter      = 20;                    % maximum step number
tol          = 1000*eps;
[m,n]        = size(B);
IN           = zeros(1,m);
IN(IB)       = ones(1,n);
IN           = find(IN == 0)
iter         = 0;
if errorcode == 0
   res        = y*B - a;
   res        = abs(res);
   if any(res > tol) | any(y < - tol)
      errorcode = 1;                       % y not feasible
   end
end
done         = errorcode;
% -- first Tableau ----------------------------------------
if errorcode == 0
   A          = inv(B(IB,:));
   D          = B(IN,:)*A;
   f          = y*c;
   w          = a*A;
   x          = A*c(IB);
   r          = D*c(IB) - c(IN);
   P          = [A  x;D  r;w  f];
end;
if all(r <= 0)
   done       = 1;
end
% ---------------------------------------------------------
while ~done,
   iter       = iter + 1;
   i          = max(find(r == max(r)));
   k          = i + n;
   d          = P(k,1:n);
   if all(d <= 0)
      errorcode = 2;                % solution does not exist
   else
      H        = find(d > 0);
      s        = w(H)./d(H);
      tau      = min(s);
      K        = find(s == tau);
      j        = H(min(K));
      % - swapping with pivot (k,j) -----------------------------
      P        = swap(P,k,j);
      % - adaption of index vectors -----------------------------
      aux      = IB(j);
      IB(j)    = IN(i);
      IN(i)    = aux;
      r        = P(n+1:m,n+1);
      w        = P(m+1,1:n);
   end
   % --------------------------------------------------------
   if iter > maxiter
      errorcode = 3;              % max. step number reached
   end
   done       = all(r <= tol) | errorcode > 0
   iter
   IB
   y
   pause
end;
% ---------------------------------------------------------
if errorcode == 0
   x          = P(1:n,n+1);
   y          = zeros(1,m);
   y(IB)      = P(m+1,1:n);
   f          = P(m+1,n+1);
else
   x          = NaN*ones(n,1);
   y          = NaN*ones(1,m);
   f          = NaN;
end
