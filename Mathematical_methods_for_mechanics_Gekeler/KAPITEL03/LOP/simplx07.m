      % --------------------------------------------------------
      function [x,y,f,errorcode] = simplx07(a,B,c,p);
      % Gekeler,Baumgarten: Linear Programming
      % simplex method in tableau form
      % problem min(y*c, y*B = a, y(i) >= 0, i = p+1,...,m)
      % phase 1 and phase 2
      % B (m,n)-matrix, a,y row vectors, c,x column vectors
      % ATTENTION: The problem is transformed into column form
      %           and PROJ05.M is modified to the present
      %           side conditions
      % Assumption:
      %           rank(B) = n
      %           rank(B(:,1:p) = p;
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
      %         min(a*x, B*x = c, [0 -I]*x <= 0,
      %         I identity of dimension m-p
      d            = c;
      c            = a';
      B            = B';
      a            = d';
      % x          = y'   (!)
      %-- first tableau -----------------------------------------
      errorcode    = 0;
      phase        = 0;
      iter         = 0;
      maxiter      = 20;
      tol          = 1000*eps;
      [n,m]        = size(B);
      mm           = m - p;
      ss           = sign(c) == - 1;
      if all(ss)
        c          = - c;
        B          = - B;
      else
        if any(ss)
          c(ss)    = - c(ss);
          B(ss,:)  = - B(ss,:);
        end;
      end;
      b            = ones(1,n)*B;
      delta        = ones(1,n)*c;
      A            = [- eye(p), zeros(p,m-p)];
      xc           = zeros(p,1);
      D            = - B;
      r            = - c;
      w            = - b;
      f            = 0;
      IB1          = [];
      IB2          = [zeros(1,p), 1:m-p];
      IN1          = mm+1:mm+n;
      IN2          = [];
      IN           = [IN1, IN2];
      q            = 0;
      h            = n;
      done         = 0;
      if (delta == 0) & (p == 0)
        done       = 1
        x          = zeros(m,1);
      end;
      l            = 1;
      P            = [A  xc;D   r;w   f];
      while phase < 2
        phase      = phase + 1
        while ~done
          iter     = iter + 1;
          r        = P(p+1:p+h,m+1);
      %-- step 1.1 ----------------------------------------------
          if any(IB2 == 0)
            v      = abs(w(q+1:m));
            K      = find(IB2 == 0);
            maxv   = max(v(K));
            L      = find(v(K) == maxv);
            j      = K(min(L));
            l      = q + j;
          end
      %-- step 1.2 ----------------------------------------------
          if any(IB2 == 0) & (w(l)~=0 | w(q+1:m) >= - tol)
            if w(l) > 0
              d    = - P(p+1:p+h,l);  % - A(:,l) search direction
            else
              d    =   P(p+1:p+h,l);  %   A(:,l) search direction
            end;
            if (abs(w(l)) <= tol) & (all(d >= 0))
              d    = - d;
            end;
          else
            H      = find(IB2 > 0);
            minw   = min(w(H+q));
            L      = find(w(H+q) == minw);
            j      = H(min(L));
            if any(r == 0)                        % Bland's rule
              L    = IB2(H);
              aux  = min(L(w(H+q) < - tol));
              j    = find(IB2 == aux);
            end;
            l      = q + j;
            d      = P(p+1:p+h, l);
          end;
      %-- step 2 ------------------------------------------------
          if d >= 0
            errorcode = 2;               %solution does not exist
          else
            H      = find(d < 0);
            s      = r(H)./d(H);
            mins   = min(s);
            K      = find(s == mins);
            i      = H(min(K));
            if any(r == 0)                         % Bland's rule
              L    = IN(H);
              i    = find(IN == min(L(K)));
            end;
            k      = p + i;
      %-- swapping with pivot (k,l) -----------------------------
            P      = swap(P,k,l);
      %-- adaption of tableau and index vectors -----------------
            aux    = IB2(j);
            if i > n - q
              IB2(j) = IN(i);
              if aux == 0
                IN2 = IN2([1:i-n+q-1, i-n+q+1:q+h-n]);
                P   = P([1:k-1, k+1:p+h+1],:);
                h   = h - 1;
              else
                IN2(i-n+q) = aux;
              end;
            else
              IB1  = [IB1, IN(i)];
              IB2  = IB2([1:j-1, j+1:m-q]);
              IN1  = IN1([1:i-1, i+1:n-q]);
              if aux == 0
                P  = P([1:k-1, k+1:p+h+1],:);
                P  = P(:,[1:q, l, q+1:l-1, l+1:m+1]);
                h  = h - 1;
              else
                IN2 = [IN2, aux];
                P   = P([1:k-1, k+1:p+h, k, p+h+1],...
                           [1:q, l, q+1:l-1, l+1:m+1]);
              end;
              q    = q + 1;
            end;
            IN     = [IN1, IN2];
            w      = P(p+h+1,1:m);
          end;  % step 2
          if iter > maxiter
            errorcode = 3;             %max. step number attained
          end;
      %-- test for optimum --------------------------------------
          done     = all(w(q+1:m) >= - tol);
          done     = done | (errorcode > 0);
          x        = zeros(m,1);
          x(1:p)   = P(1:p,m+1);
          x(p+IN2) = - P(p+n-q+1:n,m+1);
        end;  % while ~done
      %-- adaption of tableau and index vectors after phase 1----
        if phase==1
          f        = P(n+1,m+1);
          if f < delta - tol
            errorcode = 1;
            phase  = 3;
          else
            IN0    = ones(1,n - q);
            for i0 = IN0
              k0   = p + i0;
              w0   = P(k0,q+1:m);
              g    = abs(w0);
              j0   = min(find(g == max(g)));
              l0   = q + j0;
      %-- swapping with pivot (k0,l0) ---------------------------
              P    = swap(P,k0,l0);
              IB1  = [IB1, IN(i0)];
              IN2  = [IN2, IB2(j0)];
              IB2  = IB2([1:j0-1,  j0+1:m-q]);
              IN1  = IN1([1:i0-1, i0+1:n-q]);
              IN   = [IN1, IN2];
              P    = P([1:k0-1, k0+1:n, k0, n+1],...
                                [1:q, l0, q+1:l0-1, l0+1:m+1]);
              q    = q + 1;
            end; %for
            if p==0
              w1   = a(IN)*P(p+1:n,1:n);
              w2   = a(IN)*P(1:n,n+1:m) + a(IB2);
            else
              w1   = a(p+IN)*P(p+1:n,1:n) - a(1:p)*P(1:p,1:n);
              w2   = a(p+IN)*P(p+1:n,n+1:m) + a(p+IB2) ...
                                        - a(1:p)*P(1:p,n+1:m);
            end;
            w      = [w1, w2];
            f      = - a*x;
            P(n+1,:) = [w, f];
            done   = all(w(q+1:m) > - tol);
          end;
        end;
      end; % while phase < 2
      x            = zeros(m,1);
      y            = zeros(mm+n,1);
      f            = - P(n+1,m+1);
      if errorcode == 0
        x(1:p)     =   P(1:p,m+1);
        x(p+IN)    = - P(p+1:n,m+1);
        y([IB1-mm, n+IB2]) = P(n+1,1:m);
        if any(ss)
          y([ss; zeros(mm,1)]) = - y([ss; zeros(mm,1)]);
        end;
      else
        x          = NaN*ones(m,1);
        y          = NaN*ones(mm+n,1);
        f          = NaN;
      end
      % transformation to row problem ---------------------------
      u            = - y(1:n);
      y            =   x';
      x            =   u;
      % ---------------------------------------------------------
