function [x,y,f,errorcode] = proj05(a,B,c,p);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% projection method in tableau form with large tableau
% phase 1 and phase 2
% problem max(a*x, B*x <= c)
% first p inequalities are equations
% B (m,n)-Matrix, a,y row vector, c,x column vectors
% 0 <= p < n,
% ASSUMPTION:
%        rank(B(1:p,:)) = p, rank(B) = n
% OUTPUT x vertex of feasible domain
%        y Lagrange multiplier
%        f value of objective function
%        errorcode = 0: solution found
%        errorcode = 2: solution does not exist
%        errorcode = 3: max. step number attained
%        errorcode = 4: feasible domain empty
%        errorcode = 5: pivot nearly zero
%-- first tableau -----------------------------------------
maxiter = 20;
tol     = 1000*eps;
errorcode = 0; phase = 0; iter = 0; [m,n] = size(B);
mm = m; nn = n; xx0 = max([-c(p+1:mm);0]);
delta = 0; x = zeros(n,1);
if p > 0
   ss     = sign([c(1:p);zeros(mm-p,1)]);
   M      = find(ss == -1); c(M) = - c(M);
   B(M,:) = - B(M,:);      delta = ones(1,p)*c(1:p);
end
if p == 0
   b = zeros(1,n); A = eye(n); D = B;
   r = - c;        w = a;      f = 0;
else
   b     = ones(1,p)*B(1:p,:);
   delta = ones(1,p)*c(1:p);
   [Q,R] = qr(B(1:p,:)');
   Q     = Q(:,p+1:n)';
   A = [B(1:p,:); Q]; A = inv(A);
   D = B(p+1:mm,:)*A; w = a*A; r = - c(p+1:mm); f = 0;
end
IB1 = 2:p+1; IB2 = zeros(1,n-p);
IN  = p+2:mm+1;
m   = mm - p; q = p;
if -xx0 == delta
   done       = 1;
else
   %-- tableau and index vector for feasible x ---------------
   IB1 = []; IB2 = zeros(1,n+1);
   IN  = 1:mm+1;
   c   = [0 ;c]; A = eye(n+1);
   x   = [xx0; zeros(n,1)];
   D   = [-1, zeros(1,n);[zeros(p,1);-ones(mm-p,1)],B];
   r   = D(:,1)*xx0 - c; w = [-1, b]; f = - xx0;
   m   = mm+1; n = n+1; done = 0; q = 0;
end
P = [A, x; D, r;w,  f];
while phase < 2
   phase  = phase + 1;
   while ~done
      iter = iter + 1;
      l    = 1;
      %-- step 1.1 ----------------------------------------------
      if any(IB2 == 0)
         v    = abs(w(q+1:n)); K = find(IB2 == 0);
         maxv = max(v(K));
         L    = find(v(K) == maxv); j = K(min(L)); l = j + q;
      end
      r = P(n+1:n+m,n+1);
      %-- step 1.2 ----------------------------------------------
      if any(IB2 == 0) & (w(l)~=0 | w(q+1:n) >= - tol)
         if w(l) > 0
            d = - P(n+1:n+m,l);  % - A(:,l) search direction
         else
            d = P(n+1:n+m,l);    %   A(:,l) search direction
         end
         if (abs(w(l)) <= tol) & (all(d >= 0))
            d = - d;
         end
      else
         H = find(IB2 > 0); minw = min(w(H+q));
         L = find(w(H+q) == minw); j = H(min(L));
         if any(r == 0)                         % Bland's rule
            L   = IB2(H);
            aux = min(L(w(H+q) < 0));
            j   = find(IB2==aux);
         end
         l = j + q; d = P(n+1:n+m,l);
      end
      %-- step 2 -----------------------------------------------
      if d >= 0
         errorcode = 2;               %solution does not exist
      else
         H = find(d < 0);
         s = r(H)./d(H); mins = min(s);
         K = find(s == mins); i = H(min(K));
         if any(r == 0)                         % Bland's rule
            L = IN(H); i = find(IN == min(L(K)));
         end
         k = i + n;
         %-- swapping with pivot element (k,l) ---------------------
         P = swap(P,k,l);
         %-- adaption of index vectors------------------------------
         aux = IB2(j);
         if aux == 0
            P = P([1:k-1, k+1:n+m+1],:);%i = n-l is active
            IB2(j) = IN(i); IN   = IN([1:i-1, i+1:m]);
            m = m - 1;
         else
            IB2(j) = IN(i); IN(i)  = aux;
         end
         if IB2(j) == 1
            n  = n - 1; IB2  = IB2([1:j-1, j+1:n-q+1]);
            P  = P([2:n+m+2], [1:l-1 l+1:n+2]);
         else
            if (1 < IB2(j)) & (IB2(j) <= p+1)
               q = q+1;
               IB1 = [IB1, IB2(j)]; IB2(j) = IB2(1);
               IB2 = IB2(2:n-q+1);
               P(:,[q, l])= P(:,[l, q]);
            end
         end
         w = P(n+m+1,1:n);
      end  % step 2
      if iter > maxiter
         errorcode = 3;           %max. step number attained
      end
      %-- test for optimality ---------------------------------
      done = all(IB2 > 0) & all(w(q+1:n) >= - tol);
      done = done | (errorcode > 0);
      x    = P(1:n,n+1);
      % -------------------------------------------------------
      %  iter
      %  IB2
      % -------------------------------------------------------
   end  % while ~done
   %-- adaption of tableau and index vectors after phase 1 ---
   if phase == 1
      f = P(n+m+1,n+1);
      if f < delta - tol
         errorcode = 1;
         phase  = 3;
      else
         IN0 = find(IN <= p+1);
         for i0 = IN0
            k0 = i0 + n; w0 = P(k0,q+1:n); g  = abs(w0);
            j0 = min(find(g == max(g)));
            l0 = j0 + q;
            %-- swapping with pivot element (k0,l0) -------------------
            P   = swap(P,k0,l0);
            q   = q + 1;
            IB1 = [IB1, IN(i0)];
            aux = IB2(j0);
            IB2(j0) = IB2(1); IB2 = IB2(2:n-q+1);
            IN(i0)  = aux;
            P(:,[q, l0]) = P(:,[l0, q]);
         end %for
         w = a*P(1:n,1:n); f = a*P(1:n,n+1);
         P(n+m+1,:) = [w,  f];
         done   = all(IB2 > 0) & all(w(q+1:n) >= 0);
      end
   end
end % while phase < 2
if errorcode == 0
   f = P(n+m+1,n+1); x = P(1:n,n+1); y = zeros(1,n+m);
   IB1 = IB1 - 1; IB2 = IB2 - 1; IN = IN - 1;
   y([IB1 IB2]) = P(n+m+1,1:n);
   if p > 0, y(M)= - y(M); end
else
   f = NaN; x = NaN*ones(n,1); y = NaN*ones(1,n+m);
end
