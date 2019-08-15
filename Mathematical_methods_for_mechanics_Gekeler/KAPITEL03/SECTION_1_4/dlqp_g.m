function [x,y,f,errorcode,Pfad] = dlqp_g(A,a,B,b);
% Eckart Gekeler, Universitaet Stuttgart, Release 26/01/09
% Dual method for quadratic optimization 
% after Goldfarb and Idnani
% f(x) = x'*A*x/2 - a'*x = Min!,
% g(x) = B*x + b >= 0;
% INPUT A symm. positive definite (n,n)-matrix,
%       a n-column vector,
%       B (m,n)-matrix, b m-column vector
% OUTPUT x optimal solution,
%        y optimal Lagrange-multiplier for g
%        f optimal value of objective function
% -- Parameter ----------------------
maxit = 100; tol = 1.0E-7; Max = 1.0E10;
% --------------------------------------
%CONDA = condest(A)
[m,n] = size(B);
%m_n_p_r = [m,n,p,r]
errorcode = 0; it = 0; IB = []; q = 0; u = 0;
fullstep = 1;
x = A\a; Pfad = x;
f =  x.'*A*x/2 - a.'*x; g = B*x + b; ming = min(g);
done = (ming > - tol) | (errorcode > 0);
while ~done
   it = it + 1;
 %  disp('******************************')
   %- Look for inactive side condition ----------------
   if fullstep == 1
      if q == 0; u = [0]; else, u = [u;0]; end
 %     disp(' Introduce restriction s ')
       g = B*x + b; ming = min(g);
      s = min(find(g == ming));
%      disp('active Indices ');
      if q == 0, IB1 = s; else, IB1 = [IB, s]; end
      IB1
      b_s = B(s,:).';         % b_s ist Gradient
   end;
   % - Calculate solution of subproblem ---------------
   if q == 0
      d = A\b_s;  ru = 0;
      t1 = Max;
   else
      NB = B(IB,:);
      D  = [A, NB.'; NB, zeros(q,q)];
      h  = [b_s; zeros(q,1)];
      v  = D\h; d = v(1:n); ru = v(n+1:n+q);
      if max(ru) <= tol, t1 = Max; end
   end;
   % Restriction l possibly cancelled ------------------
   if q > 0 & max(ru) > tol
      L  = find(ru > 0);
      r1 = u(L)./ru(L);
      t1 = min(r1);
      l  = L(min(find(r1 == t1)));
   end;
   if norm(d) <= tol % inactivation necessary
      t2 = Max;
   else
      t2 = - (b_s.'*x + b(s))/(b_s.'*d);
   end
   t = min(t1, t2);
   if t == Max
%     disp('Solution does not exist ');
      errorcode = 2;
      return
   else
      x  = x + t*d;
  %    f  = f + t*(d.'*b_s)*(u(q+1) + (t/2));
      if q == 0, u = t; else, u = u - t*[ru; -1]; end
      if t == t2 % disp('full step ');
         if q == 0, IB = s; else, IB = [IB, s]; end;
         q = q + 1;
         fullstep  = 1;
      end;
      if t == t1  & ~isempty(l)
         disp(' Cancel restriction');
         k = IB(l); IB = IB(find(IB ~= k));
         L = 1:q+1;  L = find(L ~= l);
         u = u(L);   q = q - 1;
         fullstep = 0;
      end;
   end
   g = B*x + b; ming = min(g);
   if it > maxit, errorcode = 3; end
   done = (ming > - tol) | (errorcode > 0);
   Pfad = [Pfad,x];
   IB;
end;
% -- last step ----------------
gradf = A*x - a; NB = B(IB,:);
D  = [A, NB.'; NB, zeros(q,q)];
h  = [gradf; zeros(q,1)];
v  = D\h; d = v(1:n); u = v(n+1:n+q);
 % ----------------------------
f = x.'*A*x/2 - a.'*x;
y = zeros(m,1);
if length(IB) > 0, y(IB) = u(1:length(IB)); end   
Lagrangenorm  = norm(A*x - a - B.'*y)
if Lagrangenorm > 10*tol , errorcode = 4; end

