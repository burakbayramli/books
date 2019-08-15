function [x,y,f,errorcode] = proj06(a,B,c,p,l,u,x,errorcode1);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Revised projection method
% problem max(a*x, B*x <= c, b^ix = c^i, i = 1:p, l <= x <= u)
% phase 2: start in feasible point
% B (m,n)-matrix, a,y row vectors, c,l,u,x column vectors
% ASSUMPTION:
%       (a) rang(B(1:p,:) = p
%       (b) rang (B) = n
%       (c) - inf*ones(n,1) <= l, u <= inf*ones(n,1)
% OUTPUT:
%        x solution of primal problem
%        y solution of dual problem
%        f value of objective function
%        errorcode = 0: solution found
%        errorcode = 1: x not feasible
%        errorcode = 2: solution does not exist
%        errorcode = 3: max. step number reached
% REMARK:
%       IB is index vector C in text
%       NB is index vector D in text
%-- preparation -------------------------------------------
errorcode    = errorcode1;
iter         = 0;
maxiter      = 20;
tol          = 100*eps;                      % tolerance
[m,n]        = size(B);
I            = [1:n];
r1           = B*x - c;
r2           = l - x;
r3           = x - u;
r            = [r1; r2; r3];
if errorcode == 0
   if (p > 0) & (any(abs(r(1:p)) > tol))
      errorcode = 1;                         % x not feasible
   end
   if any(r(p+1:m+n+n) > tol)
      errorcode = 1;                         % x not feasible
   end
end
if errorcode == 0
   L          = [];
   U          = [];
   W          = 1:n;
   nl         = 0;
   nu         = 0;
   nw         = n;
   IB         = [1:p, m+1:m+n-p];
   NB         = [p+1:m];
   nb         = m - p;
   % ---------------------------------------------------------
   if p == 0
      B        = [B; eye(n)];
      c        = [c; x];
      A        = eye(n);
   else
      I        = m+1:m+n-p;
      [Q,R]    = qr(B(1:p,:)');
      B(I,:)   = Q(:,p+1:n)';
      c        = [c; B(I,:)*x];
      A        = [B(1:p,:); B(I,:)];
      A        = inv(A);
   end
   % ---------------------------------------------------------
   t          = l - u;
   w          = a*A;
end   %if errorcode == 0
done         = errorcode;
while ~done
   iter       =  iter + 1;
   sgn        = 1;
   %-- step 1.1 ----------------------------------------------
   if any(IB > m)
      v        = abs(w(p+1:nw));
      K        = find(IB(p+1:nw) > m);
      maxv     = max(v(K));
      M        = find(v(K) == maxv);
      j        = K(min(M)) + p;
      ww = w(j);
      if ww > 0
         sgn    = - 1;
      end
   end
   % -- step 1.2 ---------------------------------------------
   if all(IB <= m)
      sgn      = 1;
      v        = w(p+1:n);
      K        = find(v == min(v));
      j        = min(K) + p;
      if any(r == 0)                            %Bland's rule
         % IB1    = IB(p+1:n);
         % K      = find(v < 0);
         % k      = min(IB1(K));
         % j      = find(IB1 == k) + p;
      end
   end                                     %add Bland's rule
   %-- step 2 ------------------------------------------------
   nb         = length(NB);
   r1         = B(NB,:)*x - c(NB);
   r2         = l(W) - x(W);
   r3         = t(U);
   r4         = x(W) - u(W);
   r5         = t(L);
   r          = [r1;r2;r3;r4;r5];
   % -- Case I -----------------------------------------------
   if j <= nw
      J        = [1:j-1, j+1:nw];
      d4       = A(:,j);
      d1       = B(NB,W)*d4;
      d        = [d1; - d4; d4];
      if any(IB > m) & (abs(ww) < tol) & all(d >= 0) & any(d > 0)
         sgn    = - 1;
      end
      if all(sgn*d >= 0)
         errorcode = 2
      else
         d1     = sgn*d1;
         d4     = sgn*d4;
         h1     = find(d1 < 0);
         h2     = find(d4 > 0);
         h4     = find(d4 < 0);
         h      = [h1; h2+nb; h4+nb+nw+nu];
         d      = [d1(h1); -d4(h2); d4(h4)];
         s      = r(h)./d;
         mins   = min(s);
         K      = find(s == mins);
         i      = min(h(K));
         if     mins == inf
            if length(d1) > 0 & any(IB > m)
               d1   = - d1;
               h1   = find(d1 < 0);
               s    = r1(h1)./d1(h1);
               mins = min(s);
               K    = find(s == mins);
               i    = min(h1(K));
            else
            errorcode = 2;
         end
      end
      nr1    = 1;
   end
end
%-- Case II -----------------------------------------------
if j > nw & j <= nw + nl
   j2       = j - nw;
   J        = [1:j2-1,  j2+1:nl];
   d4       = A*B(IB,L(j2));
   if nw == 0
      d1     = - B(NB,L(j2));
   else
      d1     = B(NB,W)*d4 - B(NB,L(j2));
   end;
   d        = [d1; -d4; d4; -1];
   if all(sgn*d >= 0)
      errorcode = 2
   else
      d1     = sgn*d1;
      d4     = sgn*d4;
      h1     = find(d1 < 0);
      h2     = find(d4 > 0);
      h4     = find(d4 < 0);
      h      = [h1; h2+nb; h4+nb+nw+nu; j2+nb+nu+2*nw];
      d      = [d1(h1); -d4(h2); d4(h4); -1];
      s      = r(h)./d;
      mins   = min(s);
      if mins == inf
         errorcode = 2
      else
         K    = find(s == mins);
         i    = h(min(K));
      end;
      nr1    = 2;
   end
end;
%-- Case III ----------------------------------------------
if  j > nw + nl
    j3       = j - (nw + nl);
    J        = [1:j3-1, j3+1:nu];
    d2       = A*B(IB,U(j3));
    if nw == 0
       d1     = B(NB,U(j3));
    else
       d1     = - B(NB,W)*d2 + B(NB,U(j3));
    end;
    d        = [d1; d2; -1; -d2];
    if all(sgn*d >= 0)
       errorcode = 2
    else
       d1     = sgn*d1;
       d2     = sgn*d2;
       h1     = find(d1 < 0);
       h2     = find(d2 < 0);
       h4     = find(d2 > 0);
       h      = [h1; h2+nb; j3+nb+nw; h4+nb+nw+nu];
       d      = [d1(h1); d2(h2); -1; -d2(h4)];
       s      = r(h)./d;
       mins   = min(s);
       if mins == inf
          errorcode = 26
       else
          K    = find(s == mins);
          i    = h(min(K));
       end;
       nr1    = 3;
    end;
 end;
if errorcode == 0
   nr       = [ones(nb,1); 2*ones(nw,1); 3*ones(nu,1);...
                             4*ones(nw,1); 5*ones(nl,1)];
   nr2      = nr(i);
   %-- adaption of tableau and index vectors ---------------
   %-- case (1) --------------------------------------------
   if (nr1==1) & (nr2==1)
      i1        = i;
      b         = B(NB(i1),W)*A;
      C         = A - A(:,j)*b/b(j);
      C(:,j)    = A(:,j)/b(j);
      A         = C;
      aux       = IB(j);
      if aux > m
         IB(j)   = NB(i1);
         M       = [1:i1-1, i1+1:nb];
         NB      = NB(M);
         nb      = nb - 1;
      else
         IB(j)   = NB(i1);
         NB(i1)  = aux;
      end
   end;
   % -- case (2) ---------------------------------------------
   if (nr1==1) & (nr2==2)
      i2        = i - nb;
      K         = [1:i2-1, i2+1:nw];
      aux       = IB(j);
      if aux <= m
         NB      = [NB, aux];
         nb      = nb + 1;
      end
      IB        = IB(J);
      L         = [L, W(i2)];
      W         = W(K);
      b         = - A(i2,:);
      C         = A  - A(:,j)*b/b(j);
      A         = C(K,J);
      nw        = nw - 1;
      nl        = nl + 1;
   end;
   % -- case (3) ---------------------------------------------
   if (nr1==1) & (nr2==4)
      i4        = i - (nb+nw+nu);
      K         = [1:i4-1, i4+1:nw];
      aux       = IB(j);
      if aux <= m
         NB      = [NB,  aux];
         nb      = nb + 1;
      end
      IB        = IB(J);
      U         = [U, W(i4)];
      W         = W(K);
      b         = A(i4,:);
      C         = A  - A(:,j)*b/b(j);
      A         = C(K,J);
      nw        = nw - 1;
      nu        = nu + 1;
   end;
   % -- case (4) ---------------------------------------------
   if (nr1==2) & (nr2==1)
      i1        = i;
      K         = [1:i1-1, i1+1:nb];
      W         = [W, L(j2)];
      L         = L(J);
      IB        = [IB, NB(i1)];
      NB        = NB(K);
      nb        = nb - 1;
      A         = [A, d4; zeros(1,nw), -1];
      b         = B(IB(nw+1),W)*A;
      C         = A - A(:,nw+1)*b/b(nw+1);
      C(:,nw+1) = A(:,nw+1)/b(nw+1);
      A         = C;
      nw        = nw + 1;
      nl        = nl - 1;
   end;
   % -- case (5) ---------------------------------------------
   if (nr1==2) & (nr2==2)
      i2        = i - nb;
      aux       = W(i2);
      W(i2)     = L(j2);
      L(j2)     = aux;
      b         = d4;
      C         = A  - b*A(i2,:)/b(i2);
      C(i2,:)   = A(i2,:)/b(i2);
      A         = C;
   end;
   % -- case (6) ---------------------------------------------
   if (nr1==2) & (nr2==4)
      i4        = i - (nb+nw+nu);
      U         = [U, W(i4)];
      W(i4)     = L(j2);
      L         = L(J);
      b         = d4;
      C         = A  - b*A(i4,:)/b(i4);
      C(i4,:)   = A(i4,:)/b(i4);
      A         = C;
      nl        = nl - 1;
      nu        = nu + 1;
   end;
   % -- case(7) ----------------------------------------------
   if (nr1==2) & (nr2==5)
      U         = [U, L(j2)];
      L         = L(J);
      nl        = nl - 1;
      nu        = nu + 1;
   end;
   % -- case (8) ---------------------------------------------
   if (nr1==3) & (nr2==1)
      i1        = i;
      K         = [1:i1-1, i1+1:nb];
      IB        = [IB, NB(i1)];
      NB        = NB(K);
      nb        = nb - 1;
      W         = [W, U(j3)];
      U         = U(J);
      A         = [A - d2; zeros(1,nw) 1];
      b         = B(IB(nw+1),W)*A;
      C         = A - A(:,nw+1)*b/b(nw+1);
      C(:,nw+1) = A(:,nw+1)/b(nw+1);
      A         = C;
      nw        = nw + 1;
      nu        = nu - 1;
   end;
   % -- case (9) ---------------------------------------------
   if (nr1==3) & (nr2==2)
      i2        = i - nb;
      L         = [L, W(i2)];
      W(i2)     = U(j3);
      U         = U(J);
      b         = d2;
      C         = A  - b*A(i2,:)/b(i2);
      C(i2,:)   = A(i2,:)/b(i2);
      A         = C;
      nl        = nl + 1;
      nu        = nu - 1;
   end;
   % -- case (10) --------------------------------------------
   if (nr1==3) & (nr2==3)
      L         = [L, U(j3)];
      U         = U(J);
      nl        = nl + 1;
      nu        = nu - 1;
   end;
   % -- case (11) --------------------------------------------
   if (nr1==3) & (nr2==4)
      i4        = i - (nb + nw + nu);
      aux       = W(i4);
      W(i4)     = U(j3);
      U(j3)     = aux;
      b         = d2;
      C         = A  - b*A(i4,:)/b(i4);
      C(i4,:)   = A(i4,:)/b(i4);
      A         = C;
   end;
   % ---------------------------------------------------------
   w1          =  a(W)*A;
   if nw == 0
      w2        =  -a(L);
      w3        =   a(U);
      x(W)      = [];
   else
      x(W)      = c(IB);
      if nl == 0
         w2      = [];
      else
         w2      = w1*B(IB,L) - a(L);
         x1      = B(IB,L)*l(L);
         x(W)    = x(W)-x1;
      end;
      if nu == 0
         w3      = [];
      else
         w3      = -w1*B(IB,U) + a(U);
         x2      = B(IB,U)*u(U);
         x(W)    = x(W) - x2;
      end;
      x(W)      = A*x(W);
   end;
   w           = [w1, w2, w3];
   x(L)        = l(L);
   x(U)        = u(U);
end    %cases for errorcode == 0
if iter > maxiter
   errorcode = 3;               %max. step number reached
end;
   %-- test for optimality ---------------------------------
   done       =  all(IB <= m) & all(w(p+1:n) >= - tol);
   done       = done | (errorcode > 0);
end;  % while ~done
if errorcode == 0
   f                    = a*x;
   y                    = zeros(1,m+n+n);
   y([IB, m+L, m+n+U])  = [w1, w2, w3];
else
   f                    = NaN;
   x                    = NaN*ones(n,1);
   y                    = NaN*ones(1,m+n+n);
end;
% --------------------------------------------------------
