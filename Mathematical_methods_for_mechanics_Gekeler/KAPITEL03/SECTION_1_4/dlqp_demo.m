function [x,y,f,errorcode,PATH] = dlqp_demo(A,a,B,b);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Dual method for quadratic optimization
% after Goldfarb and Idnani
% with MONITORING for BILD02.M and BILD03.M!!!
% f(x) = x'*A*x/2 - a'*x = Min!,
% g(x) = B*x + b >= 0;
% INPUT A symm. positive definite (n,n)-matrix,
%       a n-column vector,
%       B (m,n)-matrix,
%       b m-column vector 
% OUTPUT x optimal solution
%        y optimal Lagrange-multiplier
%        f optimal value of objective function
% Parameter ------------------------
maxit = 15; Eps = 1E-10; Max = 1E10;
% ----------------------------------
[m,n] = size(B); 
errorcode = 0; it = 0; IB = []; q = 0; y = 0;
t1 = Max; flag = 1;
% - Calulate unrestricted solution and check feasibility
x = A\a; PATH = x;
f =  x'*A*x/2 - a'*x;
g = B*x + b;
gamma = min(g);
% --- Modification for demonstration ----
gamma = g(1);
% ---------------------------------
done = (gamma > - Eps);
while ~done
   %  disp('*********************************');
   it = it + 1;
   % - Look for inactive side condition ------------
   if flag == 1
      if q == 0 y = [0]; else y = [y;0]; end
      % disp(' Introduce restriction p')
      p = min(find(g == gamma));
      % disp('aktive Indices ');
      if q == 0 IB1 = p; else IB1 = [IB; p]; end
      b_p = B(p,:)';  % b_p is Gradient
   end;
     % - Calulate solution of subproblem -----------
   if q == 0
      z  = A\b_p;
      t1 = Max;
   else
      N  = B(IB,:);
      D  = [A N'; N zeros(q,q)];
      h  = [b_p; zeros(q,1)];
      u  = D\h;
      z  = u(1:n); r = u(n+1:n+q);
      if max(r) <= 0, t1 = Max; end
   end;
   % Restriction l shall possibly be cancelled --
   if q > 0 & max(r) > 0
      K  = find(r > 0);
      r1 = y(K)./r(K);
      t1 = min(r1);
      l  = K(min(find(r1 == t1)));
   end;
   if norm(z) > Eps
      t2 = - (b_p'*x + b(p))/(b_p'*z);
      t  = min(t1,t2);
      x  = x + t*z;
      f  = f + t*(z'*b_p)*(y(q+1) + (t/2));
      if q == 0, y = t; else y = y - t*[r; -1]; end
      if t1 >= t2
         disp(' full step ');
         % disp('aktive Indices');
         if q == 0, IB = p; else IB = [IB; p]; end
         q = q + 1;
         flag  = 1;
      end;
      if t1 < t2
         % disp('Cancel restriction ');
         k  = IB(l);
         % disp('aktive Indices');
         IB = IB(find(IB ~= k));
         y  = y - t1*[r; - 1];
         K  = 1:q+1;
         K  = find(K ~= l);
         y  = y(K);
         q  = q - 1;
         flag =  0;
      end;
   end;
   if norm(z) <= Eps            % Inactivation necessary
      t2 = Max;
      if t1 == Max
         disp(' Solution does not exist ');
         errorcode = 1;
      else
         % disp(' partial step dual ! ');
         disp(' Cancel restriction ');
         k = IB(l);
         % disp('aktive Indices');
         IB = IB(find(IB ~= k));
         y  = y - t1*[r; - 1];
         K  =  1:q+1;
         K  = find(K ~= l);
         y  = y(K);
         q  = q - 1;
         flag =  0;
      end;
   end;
   g = B*x + b;
   gamma = min(g);
   if it > maxit
      errorcode = 2;
   end
   done  = ((gamma > - Eps) | (errorcode > 0));
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   PATH = [PATH,x];
end;
z = zeros(m,1);
if (length(IB) > 0) & (errorcode == 0)
  z(IB) = y;
end;
y = z;
