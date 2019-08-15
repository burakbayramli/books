      % ---------------------------------------------------------
      % phase 1-phase 2 in simplex problem, column form, p = 0 --
      function [x,y,f,IBS,errorcode] = simplx08(a,B,c);
      % Gekeler: Linear Programming
      % T. Baumgarten: Diplomarbeit
      % simplex method in tableau form
      % problem  min(y*c, y*B = a, y >= 0)
      % phase 1 and phase 2
      % B (m,n)-Matrix, a,y row vectors, c,x column vectors
      % ATTENTION: The problem is transformed into column form
      %           and PROJ05.M is modified to the present
      %           side conditions
      % ASSUMPTION:
      %           rank(B) = n
      % OUTPUT
      %           x solution of primal problem
      %           y solution of dual problem (simplex problem)
      %           f value of objective function
      %           errorcode = 0: solution found
      %           errorcode = 1: feasible domain empty
      %           errorcode = 2: solution does not exist
      %           errorcode = 3: max. step number attained
      % REMARK:
      %         IB := A(y)
      %         IN := N(y complement of IB in [1:m]
      %         (y(i) = 0, i in IN)
      % Transformation into column problem ----------------------
      %         min(a*x, B*x = c, x >= 0)
      d            = c;
      c            = a';
      B            = B';
      a            = d';
      % x          = y'   (!)
      %-- first tableau -----------------------------------------
      errorcode = 0;
      phase     = 0;
      iter      = 0;
      maxiter   = 20;
      tol       = 1000*eps;
      done      = 0;
      [n,m]     = size(B);
      mm        = m;
      ss        = sign(c)==-1;
      if any(ss)
        c(ss)   = - c(ss);
        B(ss,:) = - B(ss,:);
      end;
      b         = ones(1,n)*B;
      delta     = ones(1,n)*c;
      D         = - B;
      r         = - c;
      w         = - b;
      f         = 0;
      IB1       = [];
      IB2       = 1:m;
      IN1       = mm+1:mm+n;
      IN2       = [];
      IN        = [IN1, IN2];
      q         = 0;
      if delta == 0
        done    = 1;
      end;
      P         = [D  r;w f];
      while phase < 2
        phase = phase + 1
        while ~done
          iter = iter + 1;
      %-- step 1 ------------------------------------------------
          minw  = min(w(q+1:m));
          L     = find(w(q+1:m) == minw);
          j     = min(L);
          if any(r == 0)                           % Bland's rule
            aux     = min(IB2(w(q+1:m) < 0));
            j       = find(IB2 == aux);
          end;
          k     = q + j;
          d     = P(1:n,k);
      %-- step 2 ------------------------------------------------
          if all(d >= 0)
            errorcode = 2                %solution does not exist
          else
            H        = find(d < 0);
            s        = r(H)./d(H);
            mins     = min(s);
            K        = find(s == mins);
            i        = H(min(K));
            if any(r == 0)                         % Bland's rule
              L      = IN(H);
              i      = find(IN == min(L(K)));
            end
      %-- swapping with pivot (i,k) -----------------------------
            P        = swap(P,i,k);
      %-- adaption of tableau and index vectors -----------------
            if i <= n - q
              IN2        = [IN2 IB2(j)];
              IB1        = [IB1 IN(i)];
              IB2        = IB2([1:j-1, j+1:m-q]);
              IN1        = IN1([1:i-1, i+1:n-q]);
              IN         = [IN1, IN2];
              P          = P([1:i-1, i+1:n, i, n+1],...
                           [1:q, k, q+1:k-1, k+1:m+1]);
              q          = q + 1;
            else
              aux        = IB2(j);
              IB2(j)     = IN(i);
              IN(i)      = aux;
              IN2(i-n+q) = aux;
            end;
            w            = P(n+1,1:m);
            r            = P(1:n,m+1);
          end;  % step 2
          if iter > maxiter
            errorcode = 3;             %max. step number attained
          end;
      %-- test for optimum --------------------------------------
          done = all(w(q+1:m) >= - tol);
          done = done | (errorcode > 0);
          x           = zeros(m,1);
          x(IN2)      = - P(n-q+1:n,m+1);
        end;  % while ~done
      %-- adaption of tableau and index vectors after phase 1 ---
        if phase==1
          f   = P(n+1,m+1);
          if f < delta - tol
            errorcode = 1;
            phase     = 3;
          else
            IN0   = ones(1,n-q);
            for i0 = IN0
              w0   = P(i0,q+1:m);
              g    = abs(w0);
              j0   = min(find(g == max(g)));
              k0   = q + j0;
      %-- swapping with pivot (i0,k0) ---------------------------
              [P,errorcode] = swap(P,i0,k0);
              IN2      = [IN2, IB2(j0)];
              IB1      = [IB1, IN(i0)];
              IB2      = IB2([1:j0-1, j0+1:m-q]);
              IN1      = IN1([1:i0-1 i0+1:n-q]);
              IN       = [IN1, IN2];
              P        = P([1:i0-1, i0+1:n, i0, n+1],...
                                [1:q, k0, q+1:k0-1, k0+1:m+1]);
              q        = q + 1;
            end; %for
            u          = a(IN)*P(1:n,1:n);
            w          = a(IN)*P(1:n,n+1:m) + a(IB2);
            w          = [u,  w];
            f          = - a(IN2)*x(IN);
            P(n+1,:)   = [w, f];
            done = all(w(q+1:m) > - tol);
          end;
        end;
      end; % while phase < 2
      x       = zeros(m,1);
      y       = zeros(mm+n,1);
      f       = - P(n+1,m+1);
      if errorcode == 0
        x(IN)         = - P(n-q+1:n,m+1);
        y([IB1, IB2]) =  P(n+1,1:m);
        if any(ss)
          y([ss'; zeros(mm,1)]) = - y([ss'; zeros(mm,1)]);
        end;
      else
        x      = NaN*zeros(m,1);
        y      = NaN*zeros(1,m);
        f      = NaN;
      end
      % transformation to row problem ---------------------------
      u            = y(1:n);
      y            = x';
      x            = u;
      IB           = IN;
