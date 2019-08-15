      % ---------------------------------------------------------
      function [y,IB,errorcode] = simplx04(a,B,c,p);
      % Gekeler,Baumgarten: Linear Programming
      % simplex method in tableau form
      % problem: min(y*c, y*B = a, y(i) >= 0, i = p+1,...,m)
      % phase 1: computation of extreme point
      % B (m,n)-matrix, a,y row vectors c,x column vectors
      % ASSUMPTION:
      %         rang(B(1:p,1:p) = p       (!!!)
      %         rang(B) = n
      % ---------------------------------------------------------
      % uses simplx03.m
      % ---------------------------------------------------------
      % OUTPUT:
      %         y extreme point (if exists)
      %         IB index set of row basis of y
      %         errorcode = 3: max. step number reached
      %         errorcode = 4: feasible domain empty
      % REMARK:
      %         IB := A(y);
      %         IN := N(y) complement of IB in [1:m]
      %         (y(i) = 0, i in IN)
      %
      [m,n]        = size(B);
      % transformation and start --------------------------------
      tol          = 1000*eps;
      c1           = [zeros(m,1); ones(n,1)];
      a1           = a;
      B1           = B;
      if p == 0
        ss         = sign(a1) == - 1;
        if any (ss)
          a1(ss)   = - a1(ss);
          B1(:,ss) = - B1(:,ss);
        end
        y1         = [zeros(1,m), a1];
      else
        u          = a1(1:p)/B1(1:p,1:p);
        v          = a1(p+1:n) - u*B1(1:p,p+1:n);
        v          = [zeros(1,p), v];
        ss         = sign(v) == - 1;
        if any(ss)
          v(ss)    = - v(ss);
          a1(ss)   = - a1(ss);
          B1(:,ss) = - B1(:,ss);
        end
        y1         = [u, zeros(1,m-p), zeros(1,p), v(p+1:n)];
      end
      B1           = [B1; eye(n)];
      IB1          = [[1:p] [m+p+1:m+n]]
      errorcode    = 0;
      [x,y2,f,p,IB,errorcode] = ...
                         simplx03(a1,B1,c1,p,y1,IB1,errorcode);
      y            = y2(1:m);
      v            = y2(m+1:m+n);
      M            = find(IB <= m);
      IB           = IB(M);
      IB           = complete(B,IB);
      if any(v > tol)
        errorcode  = 4;                 % feasible domain empty
      end
      % -------------------------------------------------------
