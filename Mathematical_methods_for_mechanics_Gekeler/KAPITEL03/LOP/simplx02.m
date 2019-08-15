      % ---------------------------------------------------------
      function [y,IB,errorcode] = simplx02(a,B,c);
      % Gekeler,Baumgarten: Linear Programming
      % simplex method in tableau form
      % problem min(y*c, y*B = a, y >= 0)
      % phase 1: computation of extreme point
      % B (m,n)-matrix, a,y row vectors, c,x column vectors
      % ASSUMPTION:
      %         rang(B) = n
      % ---------------------------------------------------------
      % uses simplex1.m
      % ---------------------------------------------------------
      % OUTPUT:
      %         y vertex of dual problem (if exists)
      %         IB Index set of a row basis of y
      %         errorcode = 3: max. step number reached
      %         errorcode = 4: feasible domain empty
      % REMARK:
      %         IB  := A(y)
      %         IN  := N(y) complement of IB in [1:m]
      %
      [m,n]     = size(B);
      a1        = a;
      B1        = B;
      ss        = sign(a1);
      M         = find(ss == - 1);
      a1(M)     = - a1(M);
      B1(M)     = - B1(M);
      c1        = [zeros(m,1); ones(n,1)];
      B1        = [B1; eye(n)];
      y1        = [zeros(1,m)  a1];
      IB1       = [m+1:m+n];
      errorcode = 0;
      [x,y2,f,IB,errorcode] = simplx01(a1,B1,c1,y1,IB1,errorcode);
      y         = y2(1:m);
      v         = y2(m+1:m+n);
      if any(v > 0) | any(y < 0)
        errorcode = 4;                    % feasible domain empty
      end
      % ---------------------------------------------------------
