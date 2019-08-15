      % -------------------------------------------------------------
      % -------------------------------------------------------------
      function IB = complete(B,IB);
      % Gekeler: Linear Programming
      % B (m,n)-matrix, IB index vector, q = size(IB),
      % ASSUMPTION:
      %           rank(B(IB,:)) = q, rank(B) = n;
      % B(IB) is completed to a row basis of R^n
      [m,n]        = size(B);
      p            = length(IB);
      q            = m - p;
      if p < n
        IN         = 1:m;
        IN(IB)     = zeros(1,p);
        M          = find(IN ~= 0);
        IN         = IN(M)
        [Q,R]      = qr(B(IB,:)');
        D          = Q'*B(IN,:)';
        D          = D(p+1:n,:);
        u          = diag(D'*D);
        [v,I]      = sort(u);
        IC         = IN(I(q-(n-p)+1:q));
        IB         = [IB,IC];
      end
