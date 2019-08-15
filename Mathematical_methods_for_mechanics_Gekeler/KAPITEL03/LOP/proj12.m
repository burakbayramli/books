function [x,y,f,errorcode] = proj12(a,B,c,p);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Cf. T.Baumgarten: Diplomarbeit
% projection method in tableau form with large tableau
% phase 1 and phase 2
% problem max(a'*x, B*x <= c)
% first p inequalities are equations
% B (m,n)-Matrix, a,y row vector, c,x column vectors
% 0 <= p < n,
% ASSUMPTION:
%        rank(B(1:p,:)) = p, rank(B) = n
% OUTPUT x vertex of feasible domain
%        y Lagrange multiplier
%        f value of objective function
%        errorcode = 0: solution found
%        errorcode = 2: solution doesnot exist
%        errorcode = 3: max. step number attained
%        errorcode = 4: feasible domain empty
%        errorcode = 5: pivot nearly zero
%-- first tableau ---------------------------------------------
errorcode    = 0;
phase        = 0;
iter         = 0;
maxiter      = 20;
tol          = 1000*eps;
[m,n]        = size(B);
xx0          = max([-c(p+1:m);0]);
x            = zeros(n,1);
if p > 0
   ss         = sign([c(1:p);zeros(m-p,1)]);
   M          = find(ss == -1);
   c(M)       = - c(M);
   B(M,:)     = - B(M,:);
   b          = ones(1,p)*B(1:p,:);
   delta      = ones(1,p)*c(1:p);
   [Q,R]      = qr(B(1:p,:)');
   Q          = Q(p+1:n,:);
   A          = [B(1:p,:); Q];
   A          = inv(A);
   D          = B(p+1:m,:)*A;
   w          = a*A;
   r          = - c(p+1:m);
   f          = 0;
else
   b          = zeros(1,n);
   delta      = 0;
   A          = eye(n);
   D          = B;
   r          = - c;
   w          = a;
   f          = 0;
end;
m            = m - p;
IZ           = 2:p+1;
IB           = zeros(1,n-p);
IN           = p+2:m+p+1;
JN           = n+1:m+n;
q            = p;
if delta == -xx0
   done       = 1;
else
   %-- tableau and index vector for feasible x --------------------
   IZ         = [];
   IB         = zeros(1,n+1);
   IN         = 1:m+p+1;
   JN         = n+2:n+m+2;
   c          = [0 ;c];
   A          = eye(n+1);
   x          = [xx0; zeros(n,1)];
   D          = [-1 zeros(1,n);  [zeros(p,1); -ones(m,1)] B];
   r          = D(:,1)*xx0 - c;
   w          = [-1 b];
   f          = - xx0;
   m          = m+p+1;
   n          = n+1;
   done       = 0;
   q          = 0;
end;
P            = [A  x; D  r;w  f];
while phase < 2
    phase      = phase+1
    while ~done
       iter     = iter + 1;
       %-- step 1.1 ---------------------------------------------------
       if any(IB == 0)
          ff     = abs(w(q+1:n));
          h      = find(IB == 0);
          maxf   = max(ff(h));
          L      = find(ff(h) == maxf);
          j      = h(min(L));
          k      = j+q;
       end;
       r        = P(JN,n+1);
       %-- step 1.2 ---------------------------------------------------
       if any(IB == 0) & (w(k)~=0 | w(q+1:n) >= -100*eps)
          if w(k) > 0
             d    = - P(JN,k);            % - B(:,k) search direction
          else
             d    = P(JN,k);              %   B(:,k) search direction
          end;
          if (abs(w(k)) <= tol) & (all(d >= 0))
             d    = - d;
          end
       else
       h      = find(IB > 0);
       minw   = min(w(h+q));
          j      = h(min(find(w(h+q) == minw)));
          if any(r == 0)                             % Bland's rule
             degenit = degenit+1
             IB1     = IB(h);
             pj      = min(IB1(w(h+q) < 0));
             j       = find(IB==pj);
          end;
          k      = j+q;
          d      = P(JN,k);
       end;
       %-- step 2 ----------------------------------------------------
       if d >= 0
          errorcode = 2;                   %solution does not exist
       else
          h      = find(d < 0);
          s      = r(h)./d(h);
          mins   = min(s);
          L      = find(s == mins);
          i      = h(min(L));
          if any(r == 0)                             % Bland's rule
             INhilf  = IN(h);
             hi   = min(INhilf(L));
             i    = find(IN == hi);
          end;
          l    = i + n;
          %-- swapping with pivot element (l,k) --------------------------
          [P,errorcode] = swap(P,l,k);
          %-- adaption of index vectors-----------------------------------
          pj     = IB(j);
          if pj == 0
             P    = P([1:l-1, l+1:n+m+1],:);%i = n-k is active
             IB(j) = IN(i);
             IN   = IN([1:i-1, i+1:m]);
             m    = m - 1;
             JN   = n+1:n+m;
          else
             IB(j) = IN(i);
             IN(i) = pj;
          end;
          if IB(j) == 1
             n    = n-1;
             IB   = IB([1:j-1 j+1:n-q+1]);
             JN   = n+1:n+m;
             P    = P([2:n+m+2],[1:k-1 k+1:n+2]);
          else
             if (1 < IB(j)) & (IB(j) <= p+1)
                q = q+1;
                IZ = [IZ IB(j)];
                IB(j) = IB(1);
                IB = IB(2:n-q+1);
                P(:,[q k])= P(:,[k q]);
             end;
          end;
          w      = P(n+m+1,1:n);
       end;  % step 2
       if iter > maxiter
          errorcode = 3;                 %max. step number attained
       end;
       %-- test for optimality --------------------------------------
       done     = all(IB > 0) & all(w(q+1:n) >= -1000*eps);
       done     = done | (errorcode > 0);
       x        = P(1:n,n+1);
       iter
       IB
    end;  % while ~done
    %-- adaption of tableau and index vectors after phase 1 -------
    if phase==1
       f        = P(n+m+1,n+1);
       if f < delta  -100*eps
          errorcode = 1;
          phase  = 3;
       else
          IN0    = find(IN <= p +1);
          JN0    = IN(IN0);
          for i0 = JN0
             l0   = i0+n;
             w0   = P(l0,q+1:n);
             ff   = abs(w0);
             j0   = min(find(ff == max(ff)));
             k0   = j0 +q;
        %-- swapping with pivot element (l0,k0) -----------------------
             [P,errorcode] = swap(P,l0,k0);
             q            = q+1;
             IZ           = [IZ IN(i0)];
             pi0          = IB(j0);
             IB(j0)       = IB(1);
             IB           = IB(2:n-q+1);
             IN(i0)       = pi0;
             P(:,[q k0]) = P(:,[k0 q]);
          end; %for
          w      = a*P(1:n,1:n);
          f      = a*P(1:n,n+1);
          P(n+m+1,:) = [w f];
          done   = all(IB > 0) & all(w(q+1:n) >= 0);
          k      = 1;
       end;
    end;
 end; % while phase < 2
if errorcode == 0
   f          = P(n+m+1,n+1);
   x          = P(1:n,n+1);
   y          = zeros(n+m,1);
   IZ         = IZ - 1;
   IB         = IB - 1;
   IN         = IN - 1;
   y([IZ IB]) = P(n+m+1,1:n)';
   if p > 0
      y(M)    = -y(M);
   end;
else
   f          = NaN;
   x          = NaN*ones(n,1);
   y          = NaN*ones(n+m,1);
end;
