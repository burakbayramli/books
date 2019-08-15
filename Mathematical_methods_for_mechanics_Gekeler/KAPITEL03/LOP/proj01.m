function [x,y,f,errorcode] = proj01(a,B,c);
% Gekeler,Baumgarten: Linear Programming
% projection method in tableau form
% problem: max(a*x, B*x <= c)
% phase 2: start in vertex x
% B (m,n)-Matrix, a,y row vectors, c,x column vectors
% ASSUMPTION:
%         rank(B) = n
%         x vertex with basis IB = [1,...,n]
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
[m,n] = size(B), IB = 1:n; IN = n+1:m;
iter  = 0; errorcode = 0;
maxiter = 20;                  % maximum step number
tol     = 1000*eps             % tolerance
% -- first tableau ----------------------------------------
A = inv(B(IB,:));
D = B(IN,:)*A;
x = A*c(IB);
r = B(IN,:)*x - c(IN);
w = a*A;
f = a*x;
P = [A  x;D  r;w  f];
% ---------------------------------------------------------
if any(r > tol)
   errorcode  = 1;                      % x not feasible
end
done = errorcode;
while ~done
   iter = iter + 1;
   %-- step 1 ------------------------------------------------
   K = find(w == min(w)); j = min(K); d = P(n+1:m,j);
   %-- Schritt 2 ---------------------------------------------
   if all(d >= 0)
      errorcode = 2;                % solution does not exist
   else
      H   = find(d < 0);
      s   = r(H)./d(H);
      tau = min(s);
      K   = find(s == tau);
      i1  = H(min(K));
      i   = i1 + n;
      %-- exchange step -----------------------------------------
      Q        =   P - P(:,j)*P(i,:)/P(i,j);
      Q(:,j)   =   P(:,j)/P(i,j);
      Q(i,:)   = - P(i,:)/P(i,j);
      Q(i,j)   =   1/P(i,j);
      P        =   Q;
      %-- adaption of index vectors and Lagrange multiplicator --
      aux      = IB(j);
      IB(j)    = IN(i1);
      IN(i1)   = aux;
      r        = P(n+1:m,n+1);
      w        = P(m+1,1:n);
   end
   %----------------------------------------------------------
   if iter > maxiter
      errorcode = 3;               % max. step number reached
   end
   done = (all(w >= - tol)) | (errorcode > 0)
   iter
   IB
   pause(1)
end
if errorcode == 0
   f = P(m+1,n+1); x = P(1:n,n+1); y = zeros(1,m); y(IB) = w;
else
   f = NaN*P(m+1,n+1); x = NaN*P(1:n,n+1); y = NaN*zeros(1,m);
end
