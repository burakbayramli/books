function [x,y,z,f,errorcode] = dlqp(A,a,B,b,C,c);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Dual method for quadratic optimization
% after Goldfarb and Idnani
% f(x) = x'*A*x/2 - a'*x = Min!,
% g(x) = B*x + b >= 0; h(x) = C*x + c = 0;
% INPUT A symm. positive definite (n,n)-matrix,
%       a n-column vector,
%       B (m,n)-Matrix, b m-column vector
%       C (p,n)-matrix, c p-column vector
% OUTPUT x optimal solution,
%        y optimal Lagrange-multiplier for g
%        z optimal Lagrange-multiplier for h
%        f optimal value of objective function
% -- Parameter ----------------------
maxit = 100; tol = 1.0E-7; Max = 1.0E10;
% --------------------------------------
%CONDA = condest(A)
[m,n] = size(B); [p,r] = size(C);
m_n_p_r = [m,n,p,r];
errorcode = 0; it = 0; IB = []; q = 0; y = 0;
% - Calculate solution for h(x) = 0 and check feasibility
x  = A\a;
D  = [A, C.'; C, zeros(p,p)];
h1 = A*x - a; h2 = C*x + c; h = [h1; h2];
v  = D\h; d = v(1:n); z = v(n+1:n+p);
x  = x - d; h = C*x + c; minh = min(abs(h));
if minh > tol
%  disp(' problem unsolvable ');
   errorcode  = 1;
end;
fullstep = 1;
f =  x.'*A*x/2 - a.'*x; g = B*x + b; ming = min(g);
done = (ming > - tol) | (errorcode > 0);

while ~done
   it = it + 1;
   %- Look for inactive side condition -----------------
   if fullstep == 1
      if q == 0; y = [0]; else, y = [y;0]; end
 %     disp(' Introduce restriction s')
       g = B*x + b; ming = min(g);
      s = min(find(g == ming));
%      disp('aktive Indices ');
      if q == 0, IB1 = s; else, IB1 = [IB, s]; end
      IB1;
      b_s = B(s,:).';         % b_s ist Gradient
   end;
   % - Calculate solution of subproblem -----------------
   if q == 0
      D  = [A, C.';C, zeros(p,p)];
      h  = [b_s; zeros(p,1)];
      v  = D\h; d = v(1:n); rz = v(n+1:n+p); ry = 0;
      t1 = Max;
   else
      NB = B(IB,:);
      D  = [A, C.', NB.';C, zeros(p,p+q);NB, zeros(q,p+q)];
      h  = [b_s; zeros(p+q,1)];
      v  = D\h; d = v(1:n); rz = v(n+1:n+p); ry = v(n+p+1:n+p+q);
      if max(ry) <= tol, t1 = Max; end
   end;
   % Restriction l shall possibly be cancelled --------
   if q > 0 & max(ry) > tol
      L  = find(ry > 0);
      r1 = y(L)./ry(L);
      t1 = min(r1);
      l  = L(min(find(r1 == t1)));
   end;
   if norm(d) <= tol,
      t2 = Max;
   else
      t2 = - (b_s.'*x + b(s))/(b_s.'*d);
   end
   t = min(t1,t2);
   if t == Max
%        disp('Solution does not exist ');
         errorcode = 2;
   else
      x  = x + t*d;
      f  = f + t*(d.'*b_s)*(y(q+1) + (t/2));
      if q == 0, y = t; else, y = y - t*[ry; -1]; end
      z  = z - t*rz;
      if t == t2 % disp('full step ');
         if q == 0, IB = s; else, IB = [IB, s]; end;
         q = q + 1;
         fullstep  = 1;
      end;
      if t == t1  & ~isempty(l)
      %   disp(' Cancel restriction ');
         k  = IB(l);
         IB = IB(find(IB ~= k));
         L  = 1:q+1;
         L  = find(L ~= l);
         y  = y(L);
         q  = q - 1;
         fullstep  =  0;
      end;
   end
   g = B*x + b; ming = min(g);
   if it > maxit, errorcode = 3; end
   done = (ming > - tol) | (errorcode > 0);
end;
% -- last step ----------------
gradf = A*x - a; NB = B(IB,:);
NB = B(IB,:);
D  = [A, C.', NB.';C, zeros(p,p+q);NB, zeros(q,p+q)];
h  = [gradf; zeros(p+q,1)];
v  = D\h; d = v(1:n); z = v(n+1:n+p); u = v(n+p+1:n+p+q);
% ----------------------------
f = x.'*A*x/2 - a.'*x;
y = zeros(m,1);
if length(IB) > 0, y(IB) = u(1:length(IB)); end
%Lagrangenorm  = norm(A*x - a - B.'*y - C.'*z)
%HNORM = norm(C*x+c,inf)
