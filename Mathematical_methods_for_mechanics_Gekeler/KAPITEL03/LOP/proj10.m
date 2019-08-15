function [x,z,f,IBS,errorcode] = proj6(a,C,d,p,l,u,x,IBS,errorcode);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Revised projection method
% Phase 2
% Problem Max(a'*x, C*x <= d, l <= x <= u)
% C (m,n)-matrix, a,d,l,u,x column vectors
% start in feasible point x
% 0 <= p < n,
% first p rows of Cx <= d  are equations
% ASSUMPTION:
%       (a) first p rows of C are linear independent
%       (b) rang (C) = n
%       (c) inf <= l, u <= inf
% REMARK:
%       i in IBS if C(i,:)*x = d(i)
%       and rang C(IBS,:) = lenght(IBS)
%       IB is index (column) vector C in text
%       NB is index (column) vector D in text
% OUTPUT:
%        x extreme point of feasible domain
%        z Lagrange multiplier
%        f value of objective function
%        errorcode = 0: solution found
%        errorcode = 1: x not feasible
%        errorcode = 2: solution does not exist
%        errorcode = 3: max. step number reached
%        errorcode = 4: feasible domain empty
%        errorcode = 5: data do not match
%-- preparation ------------------------------------------
iter         = 0;
maxiter      = 15;
tol          = 1000*eps;                      % tolerance
[m,n]        = size(C);
mm           = m;
I            = [1:n]';
r1           = C*x - d;
r2           = l - x;
r3           = x - u;
r            = [r1; r2; r3];
if errorcode == 0
   if (p > 0) & (any(abs(r(1:p)) > tol))
      errorcode = 1;                         % x not feasible
   end
   if any(r(p+1:m) > tol)
      errorcode = 1;                         % x not feasible
   end
end
if errorcode == 0
   L          = find(x == l);
   U          = find(x == u);
   nl         = length(L);
   nu         = length(U);
   S          = I;
   S(L)       = zeros(nl,1);
   S(U)       = zeros(nu,1);
   W          = find(S ~= 0);
   nw         = length(W);
   IB1         = [m+1:m+nw-p]';
   IB        = [[1:p]'; IB1];
   NB        = [p+1:m]';
   % --------------------------------------------------------------
   C         = [C; zeros(nw-p,p) eye(nw-p)];
   d         = [d; x(n-(nw-p)+1:n)];
   A         = [C(1:p,:); C(m+1:m+nw-p,:)];
   A         = inv(A);
   % ---------------------------------------------------------------
   t          =  l - u;
   w1         =   a(W)'*A;
   w2         =   a(W)'*A*C(IB,L) - a(L)';
   w3         = - a(W)'*A*C(IB,U) - a(U)';
   w          =   [w1  w2  w3];
end   %if errorcode == 0
done         = errorcode;
sgn          = 1;
while ~done
   iter       =  iter + 1;
   sgn        = 1;
   %-- step 1.1 ----------------------------------------------------
   if any(IB1 > mm)
      v        = abs(w(p+1:nw));
      k        = find(IB1 > mm);
      maxv     = max(v(k));
      j        = k(min(find(v(k) == maxv)));
      ww = w(j + p);
      if ww > 0
         sgn    = -1;
      end
   end
   % -- step 1.2 ---------------------------------------------------
   if all(IB1 <= mm)
      minw     = min(w(p+1:n));
      j        = min(find(w(p+1:n) == minw));
   end
   j          = j + p;                      %add Bland's rule
   %-- step 2 ------------------------------------------------------
   r1         = C(NB,:)*x - d(NB);
   r2         = l(W) - x(W);
   r3         = t(U);
   r4         = x(W) - u(W);
   r5         = t(L);
   r          = [r1;r2;r3;r4;r5];
   % -- Case I ----------------------------------------------------
   if j <= nw
      J        = [1:j-1, j+1:nw]';
      g4       = A(:,j);
      if nw == 0
         g1 = [];
      else
         g1     = C(NB,W)*g4;
      end
      g        = [g1; - g4; g4];
      if any(IB1 > mm) & (ww == 0) & (all(g >= 0))
         sgn    = - 1;
      end;
      if all(sgn*g >= 0)
         errorcode = 2
      else
         g1     = sgn*g1;
         g4     = sgn*g4;
         h1     = find(g1 < 0);
         h2     = find(g4 > 0);
         h4     = find(g4 < 0);
         h      = [h1; h2+(m-nw); h4+(m+nu)];
         g      = [g1(h1); -g4(h2); g4(h4)];
%        s      = r(h)./g;
         mins   = min(s);
         K      = find(s == mins);
         i      = h(min(K));
         %--------------------------------------------------------
         s      = r1(h1)./g1(h1);
         mins   = min(s);
         K      = find(s == mins);
         i      = h1(min(K));
         %-------------------------------------------------------
         nr1    = 1;
      end;
   end;
   %-- Case II ----------------------------------------------------
   if (j > nw) & (j <= nw + nl)
      j2       = j - nw;
      J        = [1:j2-1,  j2+1:nl]';
      g4       = A*C(IB,L(j2));
      if nw == 0
         g1     = - C(NB,L(j2));
      else
         g1     = C(NB,W)*g4 - C(NB,L(j2));
      end;
         g1       = sgn*g1;
         g4       = sgn*g4;
         h1       = find(g1 < 0);
         h2       = find(g4 > 0);
         h4       = find(g4 < 0);
         h        = [h1; h2+(m-nw); h4+(m+nu); j2+(m+nu+nw)];
         g        = [g1(h1); -g4(h2); g4(h4); -1];
         s        = r(h)./g;
         mins     = min(s);
         K        = find(s == mins);
         i        = h(min(K));
         nr1      = 2;
      end;
      %-- Case III --------------------------------------------------
      if (j > nw + nl)
         j3       = j - (nw + nl);
         J        = [1:j3-1, j3+1:nu]';
         g2       = A*C(IB,U(j3));
         if nw == 0
            g1     = C(NB,U(j3));
         else
            g1     = - C(NB,W)*g2 + C(NB,U(j3));
         end;
         g1       = sgn*g1;
         g2       = sgn*g2;
         h1       = find(g1 < 0);
         h2       = find(g2 < 0);
         h4       = find(g2 > 0);
         h        = [h1; h2+(m-nw); j3+m; h4+(m+nu)];
         g        = [g1(h1); g2(h2); -1; -g2(h4)];
         s        = r(h)./g;
         mins     = min(s);
         K        = find(s == mins);
         i        = h(min(K));
         nr1      = 3;
      end;
      if errorcode == 0
         nr = [ones(m-p,1); 2*ones(nw,1); 3*ones(nu,1);...
              4*ones(nw,1); 5*ones(nl,1)];
         nr2      = nr(i);
         %-- adaption of tableau and index vectors ------------------
         %-- case (1) -----------------------------------------------
         if (nr1==1) & (nr2==1)
            i1        = i;
            c         = C(NB(i1),:)*A;
            D         = A - A(:,j)*c/c(j);
            D(:,j)    = A(:,j)/c(j);
            A         = D;
            aux       = IB(j);
            if aux > m
               IB(j)  = NB(i1);
               LL      = 1:m-p;
               LL      = find(LL ~= i1);
               NB     = NB(LL);
               m       = m - 1;
            else
            IB(j)  = NB(i1);
            NB(i1) = aux;
         end
         IB1      = IB(p+1:nw)';
      end;
      % -- case (2) ------------------------------------------------
      if (nr1==1) & (nr2==2)
         i2        = i - (m - nw);
         K         = [1:i2-1 i2+1:nw];
         aux       = IB(j);
         if aux > m
            nq      = nq - 1;
         else
            NB     = [NB; aux];
         end
         IB       = IB(J);
         L         = [L W(i2)];
         W         = W(K);
         c         = - A(i2,:);
         D         =   A  - A(:,j)*c/c(j);
         A         =   D(K,J);
         nw        = nw-1;
         nl        = nl+1;
      end;
      % -- case (3) -------------------------------------------------
      if (nr1==1) & (nr2==4)
         i4        = i - (m + nu);
         K         = [1:i4-1 i4+1:nu]';
         aux       = IB(j);
         if aux > m
            nq = nq - 1;
         else
            NB     = [NB; ss];
         end
         IB       = IB(J)
         U         = [U; W(i4)];
         W         = W(K);
         c         = A(i4,:);
         D         = A  - A(:,j)*c/c(j);
         A         = D(K,J);
         nw        = nw-1;
         nu        = nu+1;
      end;
      % -- case (4) ---------------------------------------------------
      if (nr1==2) & (nr2==1)
         i1        = i;
         K         = [1:i1-1 i1+1:m-nw]';
         W         = [W; L(j2)];
         L         = L(J);
         IB       = [IB; NB(i1)];
         NB       = NB(K);
         A         = [A g4; zeros(1,nw) -1];
         c         = C(IB(nw+1),W)*A;
         D         = A - A(:,nw+1)*c/c(nw+1);
         D(:,nw+1) = A(:,nw+1)/c(nw+1);
         A         = D;
         nw        = nw+1;
         nl        = nl-1;
      end;
      % -- case (5) -------------------------------------------------
      if (nr1==2) & (nr2==2)
         i2        = i-(m-nw);
         aux       = W(i2);
         W(i2)     = L(J);
         L(j2)     = aux;
         c         = g4;
         D         = A  - c*A(i2,:)/c(i2);
         D(i2,:)   = A(i2,:)/c(i2);
         A         = D;
      end;
      % -- case (6) -------------------------------------------------
      if (nr1==2) & (nr2==4)
         i4        = i - (m - nu);
         U         = [U; W(i4)];
         W(i4)     = L(j2);
         L         = L(J);
         c         = g4;
         D         = A  - c*A(i4,:)/c(i4);
         D(i4,:)   = A(i4,:)/c(i4);
         A         = D;
         nl        = nl-1;
         nu        = nu+1;
      end;
      % -- case(7) --------------------------------------------------
      if (nr1==2) & (nr2==5)
         U         = [U; L(j2)];
         L         = L(J);
         nl        = nl - 1;
         nu        = nu + 1;
      end;
      % -- case (8) -------------------------------------------------
      if (nr1==3) & (nr2==1)
         i1        = i;
         K         = [1:i2-1 i2+1:m-nw];
         IB       = [IB; NB(i1)];
         NB       = NB(K);
         W         = [W; U(j3)];
         U         = U(J);
         A         = [A - g2; zeros(1,nw) 1];
         c         = C(IB(nw+1),W)*A;
         D         = A - A(:,nw+1)*c/c(nw+1);
         D(:,nw+1) = A(:,nw+1)/c(nw+1);
         A         = D;
         nw        = nw + 1;
         nu        = nu - 1;
      end;
      % -- case (9) -------------------------------------------------
      if (nr1==3) & (nr2==2)
         i2        = i - (m - nw);
         L         = [L; W(i2)];
         W(i2)     = U(j3);
         U         = U(J);
         c         = g2;
         D         = A  - c*A(i2,:)/c(i2);
         D(i2,:)   = A(i2,:)/c(i2);
         A         = D;
         nl        = nl+1;
         nu        = nu-1;
      end;
      % -- case (10) ------------------------------------------------
      if (nr1==3) & (nr2==4)
         L         = [L; U(j3)];
         U         = U(J);
         nl        = nl + 1;
         nu        = nu - 1;
      end;
      % -- case (11) ------------------------------------------------
      if (nr1==3) & (nr2==3)
         i3        = i - m;
         aux       = W(i3);
         W(i3)     = U(j3);
         U(j3)     = aux;
         c         = g2;
         D         = A  - c*A(i3,:)/c(i3);
         D(i3,:)   = A(i3,:)/c(i3);
         A         = D;
      end;
      % -------------------------------------------------------------
      w1          =  a(W)'*A;
      if nw == 0
         w2        =  -a(L)';
         w3        =   a(U)';
         x(W)      = [];
      else
         if nq == 0
            x(W)    = d(IB);
         else
            x(W)    = zeros(nw,1);
         end
         if nl == 0
            w2      = [];
         else
            w2      = w1*C(IB,L) - a(L)';
            x1      = C(IB,L)*l(L);
            x(W)    = x(W)-x1;
         end;
         if nu == 0
            w3      = [];
         else
            yU      = -w1*C(IB,U) + a(U)';
            x2      = C(IB,U)*u(U);
            x(W)    = x(W) - x2;
         end;
         x(W)      = A*x(W);
      end;
      w           = [w1 w2 w3];
      x(L)        = l(L);
      x(U)        = u(U);
   end    %cases for errorcode == 0
   if iter > maxiter
      errorcode = 3;                 %max. step number reached
   end;
   %-- test for optimality ---------------------------------------
   done =  (all(IB <= mm)) & (all(w(p+1:n) >= - tol));
   done = done | (errorcode > 0);
   %------------------------------------------------------
   iter
   x = (C(IB,:)\d(IB))
   f    = a'*x
   IBB =  IB'
   w
   pause
   %-----------------------------------------------------
end;  % while ~done
f               = a'*x;
z               = zeros(mm + n + n,1);
if errorcode == 0
%  z([IB m+L m+n+U])  = [w1 w2 w3]';
   z([IB mm+L mm+n+U])  = [w1 w2 w3]';
end;
