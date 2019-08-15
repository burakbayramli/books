      % --------------------------------------------------------
      function [x,y,f,IB,errorcode] = simplx05(a,B,c);
      % Gekeler,Baumgarten: Linear Programming
      % simplex method in tableau form
      % phase 1 and phase 2
      % problem   min(y*c, y*B = a, y >= 0)
      % B (m,n)-Matrix, a,y row vectors, c,x column vectors
      % ASSUMPTION:
      %           rank(B) = n
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
      %-- first tableau ----------------------------------------
      maxiter      = 20;
      tol          = 1000*eps;
      errorcode    = 0;
      phase        = 0;
      iter         = 0;
      done         = 0;
      [m,n]        = size(B);
      ss           = sign(a) == -1;
      if any(ss)
        a(ss)      = - a(ss);
        B(:,ss)    = - B(:,ss);
      end;
      b            = B*ones(n,1);
      delta        = a*ones(n,1);
      D            = B;
      r            = a;
      w            = b;
      f            = 0;
      IB1          = [];
      IB2          = 1:m;
      IN1          = m+1:m+n;
      IN2          = [];
      IN           = [IN1, IN2];
      q            = 0;
      if delta == 0
        done       = 1;
      end;
      %done        = errorcode;
      P            = [D   w;r   f];
      while phase < 2
        phase      = phase + 1
        while ~done
          iter     = iter + 1;
      %-- step 1 ------------------------------------------------
          maxw     = max(w(q+1:m));
          j        = max(find(w(q+1:m) == maxw));
          if any(r == 0)                           % Bland's rule
            K      = IB2(w(q+1:m) > 0);
            j      = find(IB2 == max(K));
          end;
          k        = q + j;
          d        = P(k,1:n);
      %-- step 2 ------------------------------------------------
          if all(d <= 0)
            errorcode = 2;              % solution does not exist
          else
            H      = find(d > 0);
            s      = r(H)./d(H);
            tau    = min(s);
            K      = find(s == tau);
            i      = H(min(K));
            if any(r == 0)                         % Bland's rule
              L    = IN(H);
              i    = find(IN == min(L(K)));
            end;
      %-- swapping with pivot (k,i) -----------------------------
            P      = swap(P,k,i);
      %-- adaption of tableau an index vectors ------------------
            if i <=  n-q
              IN2  = [IN2, IB2(j)];
              IB1  = [IB1, IN(i)];
              IB2   = IB2([1:j-1, j+1:m-q]);
              IN1  = IN1([1:i-1, i+1:n-q]);
              IN   = [IN1, IN2];
              P    = P([1:q, k, q+1:k-1, k+1:m+1],...
                             [1:i-1, i+1:n, i, n+1]);
              q    = q + 1;
            else
              aux        = IB2(j);
              IB2(j)     = IN(i);
              IN(i)      = aux;
              IN2(i-n+q) = aux;
            end;
            w      = P(1:m,n+1);
            r      = P(m+1,1:n);
          end;  % step 2
          if iter > maxiter
            errorcode = 3;             %max. step number attained
          end;
      %-- test for optimum --------------------------------------
          done     = all(w(q+1:m) <= tol);
          done     = done | (errorcode > 0);
      % ---------------------------------------------------------
          iter
          IB2
          IN
      % ---------------------------------------------------------
          y        = zeros(1,m);
          y(IN2)   = P(m+1,n-q+1:n);
        end;  % while ~done
      %-- adaption of tableau and index vectors after phase 1 ---
        if phase == 1
          f        = P(m+1,n+1);
          if f > - delta + tol
            errorcode = 1;
            phase  = 3;
          else
            IN0    = ones(1,n-q);
            for i0 = IN0
              w0   = P(q+1:m,i0);
              g    = abs(w0);
              j0   = min(find(g == max(g)));
              k0   = q + j0;
      %-- swapping with pivot (k0,i0) ---------------------------
              P    = swap(P,k0,i0);
      % ---------------------------------------------------------
              IN2  = [IN2, IB2(j0)];
              IB1  = [IB1, IN(i0)];
              IB2  = IB2([1:j0-1, j0+1:m-q]);
              IN1  = IN1([1:i0-1, i0+1:n-q]);
              IN   = [IN1, IN2];
              P    = P([1:q, k0, q+1:k0-1, k0+1:m+1],...
                            [1:i0-1, i0+1:n, i0, n+1]);
              q    = q + 1;
            end; %for
            u      = P(1:n,1:n)*c(IN);
            v      = P(n+1:m,1:n)*c(IN) - c(IB2);
            w      = [u; v];
            f      = y(IN)*c(IN2);
            P(:,n+1) = [w; f];
            done   = all(w(q+1:m) <= tol);
          end;
        end;
      end; % while phase < 2
      y            = zeros(1,m);
      w            = zeros(m+n,1);
      f            = P(m+1,n+1);
      IB           = IN;
      if errorcode == 0
        y(IN)      = P(m+1,n-q+1:n);
        w([IB1, IB2]) =  P(1:m,n+1);
        if any(ss)
          w([ss'; zeros(m,1)]) = -w([ss'; zeros(m,1)]);
        end;
        x          = - w(1:n);
      else
        x          = NaN*ones(n,1);
        y          = NaN*ones(1,m);
        f          = NaN;
      end
      % --------------------------------------------------------
