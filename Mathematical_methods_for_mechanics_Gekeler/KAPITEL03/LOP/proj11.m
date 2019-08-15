function [x,z,f,IB1,errorcode] = proj6a(a,C,d,p,l,u,x,IB1,errorcode);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
      % Revised projection method
      % Phase 2
      % Problem Max(a'*x, C*x <= d, l <= x <= u)
      % C (m,n)-matrix, a,d,l,u,x column vectors
      % start in extreme point x
      % 0 <= p < n,
      % first p rows of Cx <= d  are equations
      % ASSUMPTION:
      %       (a) first p rows of C are linear independent
      %       (b) rang (C) = n
      %       (c) inf <= l, u <= inf
      % REMARK:
      %       IB(x) Basis of extreme point x
      %       IB1 := (i in IB(x): C(i,:)*x = d(i))
      %       (1,...p) in IB1
      %       IB1 is index (column) vector C in text
      %       NB1 is index (column) vector D in text
      % OUTPUT:
      %        x extreme point of feasible domain
      %        z Lagrange muliplier
      %        f value of objective function
      %        errorcode = 0: solution found
      %        errorcode = 1: x not feasible
      %        errorcode = 2: solution does not exist
      %        errorcode = 3: max. step number reached
      %        errorcode = 4: feasible domain empty
      %        errorcode = 5: data do not match
      %-- preparation ------------------------------------------
      iter       = 0;
      maxiter    = 40;
      tol        = 1000*eps;                      % tolerance
      [m,n]      = size(C);
      I          = [1:n]';
      r1         = C*x - d;
      r2         = l - x;
      r3         = x - u;
      r          = [r1; r2; r3];
      if errorcode == 0
        if (p > 0) & (any(abs(r(1:p)) > tol))
          errorcode = 1;                          % x not feasible
        end
        if any(r(p+1:m) > tol)
          errorcode = 1;                          % x not feasible
        end
        L        = find(x == l);
        U        = find(x == u);
        nl       = length(L);
        nu       = length(U);
        S        = I;
        S(L)     = zeros(nl,1);
        S(U)     = zeros(nu,1);
        W        = find(S ~= 0);
        nw       = length(W);
        nc       = length(IB1);                   % nc = nw must hold
        if nc ~= nw
          errorcode = 5;                          % data do not match
        end
        NB1      = ones(m,1);
        NB1(IB1) = zeros(nc,1);
        NB1      = find(NB1 ~= 0);
        t        =  l - u;
      %-----------------------------------------------------------------
        A        = inv(C(IB1,W));
      %-----------------------------------------------------------------
        w1         =   a(W)'*A;
        w2         =   a(W)'*A*C(IB1,L) - a(L)';
        w3         = - a(W)'*A*C(IB1,U) - a(U)';
        w          =   [w1  w2  w3];
      end
      done = errorcode;

      while ~done
        iter     =  iter + 1;
      %-- Schritt 1 --------------------------------------------------
        minw     = min(w(p+1:n));
        j        = min(find(w(p+1:n) == minw));
        j        = j + p;                       %add Bland's rule
      %-- Schritt 2 --------------------------------------------------
        r1       = C(NB1,:)*x - d(NB1);
        r2       = l(W) - x(W);
        r3       = t(U);
        r4       = x(W) - u(W);
        r5       = t(L);
        r        = [r1;r2;r3;r4;r5];
      % -- Case I ----------------------------------------------------
        if j <= nc
          J      = [1:j-1, j+1:nc]';
          g4     = A(:,j);
          g1     = C(NB1,W)*g4;
          h1     = find(g1 < 0);
          h2     = find(g4 > 0);
          h4     = find(g4 < 0);
          h      = [h1; h2+(m-nc); h4+(m+nu)]
          g      = [g1(h1); - g4(h2); g4(h4)]
          s      = r(h)./g;
          mins   = min(s)
          K      = find(s == mins);
          i      = h(min(K));
          nr1    = 1
        end;
      %-- Case II ----------------------------------------------------
        if (j > nc)  & (j <= nc + nl)
          j2     = j - nc;
          J      = [1:j2-1,  j2+1:nl]';
          g4     = A*C(IB1,L(j2));
          if nc == 0
            g1   = - C(NB1,L(j2));
          else
            g1   = C(NB1,W)*g4 - C(NB1,L(j2));
          end;
          h1     = find(g1 < 0);
          h2     = find(g4 > 0);
          h4     = find(g4 < 0);
          h      = [h1; h2+(m-nc); h4+(m+nu); j2+(m+nu+nc)];
          g      = [g1(h1); -g4(h2); g4(h4); -1];
          s      = r(h)./g;
          mins   = min(s);
          K      = find(s == mins);
          i      = h(min(K));
          nr1    = 2
        end;
      %-- Case III --------------------------------------------------
        if (j > nc + nl)
          j3     = j - (nc + nl);
          J      = [1:j3-1, j3+1:nu]';
          g2     = A*C(IB1,U(j3));
          if nc == 0
            g1   = C(NB1,U(j3));
          else
            g1   = - C(NB1,W)*g2 + C(NB1,U(j3));
          end;
          h1     = find(g1 < 0);
          h2     = find(g2 < 0);
          h4     = find(g2 > 0);
          h      = [h1; h2+(m-nc); j3+m; h4+(m+nu)];
          g      = [g1(h1); g2(h2); -1; -g2(h4)];
          s      = r(h)./g;
          mins   = min(s);
          K      = find(s == mins);
          i      = h(min(K));
          nr1    = 3
        end;
        nr = [ones(m-nc,1); 2*ones(nc,1); 3*ones(nu,1);...
              4*ones(nc,1); 5*ones(nl,1)];
        nr2  = nr(i)
        %-- adaption of tableau and index vectors ------------------
        %-- case (1) -----------------------------------------------
        if (nr1==1) & (nr2==1)
          i1        = i;
          aux       = IB1(j);
          IB1(j)    = NB1(i1);
          NB1(i1)   = aux;
          c         = C(IB1(j),:)*A;
          D         = A - A(:,j)*c/c(j);
          D(:,j)    = A(:,j)/c(j);
          A         = D;
        end;
      % -- case (2) ------------------------------------------------
        if (nr1==1) & (nr2==2)
          i2        = i - (m - nc);
          K         = [1:i2-1 i2+1:nc];
          NB1       = [NB1; IB1(j)];
          IB1       = IB1(J);
          L         = [L W(i2)];
          W         = W(K);
          c         = - A(i2,:);
          D         = A  - A(:,j)*c/c(j);
          A         = D(K,J);
          nc        = nc-1;
          nl        = nl+1;
        end;
      % -- case (3) -------------------------------------------------
        if (nr1==1) & (nr2==4)
          i4        = i - (m + nu);
          K         = [1:i4-1 i4+1:nu]';
          NB1       = [NB1; IB1(j)];
          IB1       = IB1(J)
          U         = [U; W(i4)];
          W         = W(K);
          c         = A(i4,:);
          D         = A  - A(:,j)*c/c(j);
          A         = D(K,J);
          nc        = nc-1;
          nu        = nu+1;
        end;
      % -- case (4) ---------------------------------------------------
        if (nr1==2) & (nr2==1)
          i1        = i;
          K         = [1:i1-1 i1+1:m-nc]';
          W         = [W; L(j2)];
          L         = L(J);
          IB1       = [IB1; NB1(i1)];
          NB1       = NB1(K);
          A         = [A g4; zeros(1,nc) -1];
          c         = C(IB1(nc+1),W)*A;
          D         = A - A(:,nc+1)*c/c(nc+1);
          D(:,nc+1) = A(:,nc+1)/c(nc+1);
          A         = D;
          nc        = nc+1;
          nl        = nl-1;
        end;
      % -- case (5) -------------------------------------------------
        if (nr1==2) & (nr2==2)
          i2        = i-(m-nc);
          aux       = W(i2);
          W(i2)     = L(J);
          L(j2)      = aux;
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
          K         = [1:i2-1 i2+1:m-nc];
          IB1       = [IB1; NB1(i1)];
          NB1       = NB1(K);
          W         = [W; U(j3)];
          U         = U(J);
          A         = [A - g2; zeros(1,nc) 1];
          c         = C(IB1(nc+1),W)*A;
          D         = A - A(:,nc+1)*c/c(nc+1);
          D(:,nc+1) = A(:,nc+1)/c(nc+1);
          A         = D;
          nc        = nc + 1;
          nu        = nu - 1;
        end;
      % -- case (9) -------------------------------------------------
        if (nr1==3) & (nr2==2)
          i2        = i - (m - nc);
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
        if nc == 0
          w2        =  -a(L)';
          w3        =   a(U)';
          x(W) = [];
        else
          x(W)      = d(IB1);
          if nl == 0
            w2      = [];
          else
            w2      = w1*C(IB1,L) - a(L)';
            x1      = C(IB1,L)*l(L);
            x(W)   = x(W)-x1;
          end;
          if nu == 0
            w3      = [];
          else
            yU      = -w1*C(IB1,U) + a(U)';
            x2      = C(IB1,U)*u(U);
            x(W)    = x(W) - x2;
          end;
          x(W)     = A*x(W);
        end;
        w           = [w1 w2 w3];
        x(L)        = l(L);
        x(U)        = u(U);
        x
        if iter > maxiter
          errorcode = 3;                 %max. step number reached
        end;
        %-- test for optimality ---------------------------------------
        done =  all(w(p+1:n) >= - tol);
        done = done | (errorcode > 0);
        iter
        W
        L
        U
        IB1
        pause
      end;  % while ~done
      f             = a' *x;
      z             = zeros(m + n + n,1);
      if errorcode == 0
        z([IB1 m+L m+n+U])  = [w1 w2 w3]';
      end;
