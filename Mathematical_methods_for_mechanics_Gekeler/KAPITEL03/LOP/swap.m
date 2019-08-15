      %--------------------------------------------------------------
      %--------------------------------------------------------------
      function Q = swap(P,i,j);
      % Gekeler: Linear Programming
      %-- exchange step ---------------------------------------------
      Q            = P;
      tol          = 1000*eps;
      if abs(P(i,j)) >= tol;
        Q          =   P - P(:,j)*P(i,:)/P(i,j);
        Q(:,j)     =   P(:,j)/P(i,j);
        Q(i,:)     = - P(i,:)/P(i,j);
        Q(i,j)     =   1/P(i,j);
      else
        errorcode = 5;
      end
