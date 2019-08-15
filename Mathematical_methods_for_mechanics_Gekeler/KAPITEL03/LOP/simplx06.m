      % ---------------------------------------------------------
      function [x,y,f,errorcode] = simplx06(a,B,c,p);
      % Gekeler,Baumgarten: Linear Programming
      % simplex method in tableau form
      % phase 1 and phase 2
      % problem min(y*c, y*B = a, y(i) >= 0, i = p+1,...,m)
      % B (m,n)-matrix, a,y row vectors  c,x column vectors
      % ASSUMPTION:
      %           rank(B) = n
      %           rank(B(1:p,:) = p;
      % OUTPUT
      %           x solution of primal problem (if exists)
      %           y solution of dual problem (simplex problem)
      %           f value of objective function
      %           errorcode = 0: solution found
      %           errorcode = 1: feasible domain empty
      %           errorcode = 2: solution does not exist
      %           errorcode = 3: max. step number attained
      % REMARK:
      %         IB := A(y)
      %         IN := N(y) complement of IB in [1:m]
      %         (y(i) = 0, i in IN)
      %
      %-- first tableau -----------------------------------------
      errorcode    = 0;
      phase        = 0;
      iter         = 0;
      maxiter      = 20;
      tol          = 1000*eps;
      [m,n]        = size(B);
      ss           = sign(a)==-1;
      if all(ss)
        a          = - a;
        B          = - B;
      else
        if any(ss)
          a(ss)    = - a(ss);
          B(:,ss)  = - B(:,ss);
        end;
      end;
      b            = B*ones(n,1);
      delta        = a*ones(n,1);
      A            = [eye(p); zeros(m-p,p)];
      y1           = zeros(1,p);
      D            = B;
      r            = a;
      w            = b;
      f            = 0;
      IB1          = [];
      IB2          = [zeros(1,p), 1:m-p];
      IN1          = m-p+1:m-p+n;
      IN2          = [];
      IN           = [IN1, IN2];
      q            = 0;
      h            = n;
      done         = 0;
      if (delta == 0) & (p == 0)
        done       = 1
        y          = zeros(1,m);
      end;
      l            = 1;
      P            = [A,  D,  w;y1,  r,  f];
      while phase < 2
        phase      = phase + 1
        while ~done
          iter     = iter + 1;
          r        = P(m+1,p+1:p+h);
      %-- step 1.1 ----------------------------------------------
          if any(IB2 == 0)
            v      = abs(w(q+1:m));
            K      = find(IB2 == 0);
            maxv   = max(v(K));
            L      = find(v(K) == maxv);
            j      = K(min(L));
            l      = j + q;
          end
      %-- step 1.2 ----------------------------------------------
          if any(IB2 == 0) & (w(l) ~= 0 | w(q+1:m) <= tol)
            if w(l) < 0
              d    = - P(l,p+1:p+h);  % - A(:,l) search direction
            else
              d    =   P(l,p+1:p+h);  %   A(:,l) search direction
            end;
            if (abs(w(l)) >= - tol) & (all(d <= 0))
              d    = - d;
            end;
          else
            H      = find(IB2 > 0);
            maxw   = max(w(H+q));
            L      = find(w(H+q) == maxw);
            j      = H(max(L));
            if any(r == 0)                         % Bland's rule
              L    = IB2(H);
              aux  = min(L(w(H+q) > tol));
              j    = find(IB2 == aux);
            end;
            l      = q + j;
            d      = P(l,p+1:p+h);
          end;
      %-- step 2 ------------------------------------------------
          if all(d <= 0)
            errorcode = 2;               %solution does not exist
          else
            H      = find(d > 0);
            s      = r(H)./d(H);
            mins   = min(s);
            K      = find(s == mins);
            i      = H(min(K));
            if any(r == 0)                         % Bland's rule
              L    = IN(H);
              i    = find(IN == min(L(K)));
            end;
            k      = p + i;
      %-- swapping with pivot (l,k) -----------------------------
            P      = swap(P,l,k);
      %-- adaption of tableau and index vectors -----------------
            aux      = IB2(j);
            if i <= n - q
              IB1    = [IB1, IN(i)];
              IB2    = IB2([1:j-1, j+1:m-q]);
              IN1    = IN1([1:i-1, i+1:n-q]);
              if aux == 0
                P    = P(:,[1:k-1, k+1:p+h+1]);
                P    = P([1:q, l, q+1:l-1, l+1:m+1],:);
                h    = h - 1;
              else
                IN2  = [IN2, aux];
                P    = P([1:q, l, q+1:l-1, l+1:m+1],...
                            [1:k-1, k+1:p+h, k, p+h+1]);
              end;
              q      = q + 1;
            else
              IB2(j) = IN(i);
              if aux == 0
                IN2  = IN2([1:i-n+q-1, i-n+q+1:q+h-n]);
                P    = P(:,[1:k-1, k+1:p+h+1]);
                h    = h - 1;
              else
                IN2(i-n+q) = aux;
              end;
            end;
            IN       = [IN1, IN2];
            w        = P(1:m,p+h+1);
          end;  % Schritt 2
          if iter > maxiter
            errorcode = 3;             %max. step number attained
          end;
      %-- test for optimum --------------------------------------
          done     = all(w(q+1:m) <= tol);
          done     = done | (errorcode > 0);
          y        =   zeros(1,m);
          y(1:p)   = - P(m+1,1:p);
          y(p+IN2) =   P(m+1,p+n-q+1:n);
        end;  % while ~done
      %-- adaption of tableau and index vectors after phase 1----
        if phase == 1
          f        = P(m+1,n+1);
          if f > - delta + tol
            errorcode = 1;
            phase  = 3;
          else
            IN0    = ones(1,n-q);
            for i0 = IN0
              k0   = p + i0;
              w0   = P(q+1:m,k0);
              g    = abs(w0);
              j0   = min(find(g == max(g)));
              l0   = q + j0;
      %-- swapping with pivot (l0,k0) ---------------------------
              P    = swap(P,l0,k0);
      % ---------------------------------------------------------
              IB1  = [IB1, IN(i0)];
              IN2  = [IN2, IB2(j0)];
              IB2  = IB2([1:j0-1,  j0+1:m-q]);
              IN1  = IN1([1:i0-1, i0+1:n-q]);
              IN   = [IN1, IN2];
              P    = P([1:q, l0, q+1:l0-1, l0+1:m+1],...
                           [1:k0-1, k0+1:n, k0, n+1]);
              q    = q + 1;
            end; %for
            if p==0
              w1   = P(1:n,p+1:n)*c(IN);
              w2   = P(n+1:m,1:n)*c(IN) - c(IB2);
            else
              w1   = P(1:n,p+1:n)*c(p+IN)  - P(1:n,1:p)*c(1:p);
              w2   = P(n+1:m,p+1:n)*c(p+IN) - c(p+IB2) ...
                                       -  P(n+1:m,1:p)*c(1:p);
            end;
            w      = [w1; w2];
            f      =  y*c;
            P(:,n+1) = [w; f];
            done   = all(w(q+1:m) <= tol);
          end;
        end;
      end; % while phase < 2
      y            = zeros(1,m);
      z            = zeros(m-p+n,1);
      f            = P(m+1,n+1);
      if errorcode == 0
        y(1:p)     = - P(m+1,1:p);
        y(p+IN)    = P(m+1,p+1:n);
        z([IB1-(m-p), n+IB2]) = P(1:m,n+1);
        if any(ss)
          z([ss'; zeros(m-p,1)]) = - z([ss'; zeros(m-p,1)]);
        end;
        x          = z(1:n);
      else
        x          = NaN*ones(n,1);
        y          = NaN*ones(1,m);
        f          = NaN;
      end
      % --------------------------------------------------------
