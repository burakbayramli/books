function [x1,f,errorcode] = sqp(funfcn,x,Parsqp,Parmeter);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Method of sequential quadratic optimization
% Cf. Spellucci, \S 3.6
% f(x) = Min!, g(x) >= 0, h(x) = 0
% FUNCTIONS: dlqp.m
% INPUT  funfcn:   Name of the problem
%        x:        Arbitrary point as start vector
%        Tol:      Tolerance
%        Maxit:    Maximal step number
%        Parmeter: Additional Parameters
% OUTPUT x Solution in case of success
%        f Value of objective function
%        errorcode = 1: Max. step number in iteration
%        errorcode = 2: Max. step numberin backtracking
%        errorcode = 3: Max. step number in QP-adaption
% Modifications:
% beschl = 1: A = I, Method of ROSEN
% beschl = 2: in convex problems
% beschl = 3: helps sometimes in non-convex problems
% --------------------
% PARAMETER ----------
Tol = Parsqp(1);       Maxit = Parsqp(2);
epsilon = Parsqp(3); beschl  = Parsqp(4);
BeginMaratos = Parsqp(5);
MaxitGA = 40;
MaxitQP = 30;
Eps  = 1.0E-10;    %problem related
Beps = 1.0E-7;     %problem related
alpha = 0.5; delta = 0.1; eta = 0.05; BFGS = 1;
%----------------------------------------------------------
f0     = feval(funfcn,x,1,Parmeter);
gradf1 = feval(funfcn,x,4,Parmeter);
g0     = feval(funfcn,x,2,Parmeter);
gradg1 = feval(funfcn,x,5,Parmeter);
h0     = feval(funfcn,x,3,Parmeter);
gradh1 = feval(funfcn,x,6,Parmeter);
n = length(x); m = length(g0); p = length(h0);
zero_m = zeros(m,1); zero_n = zeros(n,1);
zero_p = zeros(p,1); one_m = ones(m,1); one_p = ones(p,1);
G0    = min(g0,zero_m);
beta  = epsilon*one_m; gamma = epsilon*one_p;
y = beta; z = gamma; d = zero_n;
tau1 = max(- min(g0),1);
tau2 = max(max(abs(h0)),1);
tau  = max(tau1,tau2);
A    = eye(n);
errorcode = 0; done = 0; iter = 0; zz = 1;

while ~done
   iter  = iter + 1;
   f     = feval(funfcn,x,1,Parmeter);
   g     = feval(funfcn,x,2,Parmeter);
   G1    = min(g,0);
   h     = feval(funfcn,x,3,Parmeter);
   gradf = gradf1;
   gradg = gradg1;
   gradh = gradh1;
   gradl = gradf - y'*gradg - z'*gradh;
   % -- Data for quadratic subproblem -------
   ecode = 0; xi = 10; it_qp = 0; doneQP = 0;
   while ~doneQP
      xi = xi/10; it_qp = it_qp + 1;
      a  = - gradf';
      IA = find(g <= Eps); IB = find(g > Eps);
      B  = [gradg(IA,:); gradg(IB,:)];
      b  = [xi*g(IA); g(IB)];
      C  = gradh; c  = xi*h;
      [d,v,z1,ff,ecode] = dlqp(A,a,B,b,C,c);
      doneQP = (~ecode) | (it_qp > MaxitQP);
      if it_qp > MaxitQP
      %  disp('Max. step number in QP-adaption ');
         ecode, errorcode = 3;
      end
   end
   y1 = zero_m; l1 = length(IA); l2 = length(IB);
   if l1 > 0, y1(IA) = v(1:l1); end
   if l2 > 0, y1(IB) = v(l1+1:l1+l2); end
   % -- Adaption of penalty vectors -------------------
   beta1  = y1 + epsilon*one_m;
   gamma1 = abs(z1) + epsilon*one_p;
   theta  = 1;
   if (min(beta1 - beta) >= 0) & (min(gamma1 - gamma) >= 0)
      theta  = 0;
   end
   pen0 = f0 - beta1'*G0 + gamma1'*abs(h0);
   pen1 = f  - beta1'*G1 + gamma1'*abs(h);
   pendiff = pen0 - pen1;
   if (theta == 1) & (pendiff >= zz*eta);
      zz = zz + 1; beta = beta1; gamma = gamma1;
   else
      K = find(y1 + epsilon*one_m >= beta);
      if length(K) > 0
         beta(K) = y1(K) + 2*epsilon*ones(length(K),1);
      end
      K = find(abs(z1) + epsilon*one_p >= gamma);
      if length(K) > 0
         gamma(K) = abs(z1(K)) + 2*epsilon*ones(length(K),1);
      end;
   end
   % -- Backtracking Strategy -----------------------
   flag1 = (any(g < - 0.5*tau));
   flag2 = (any(abs(h) > 0.5*tau));
   flag  = flag1 | flag2;
   if all(G1 >= - Eps)
      gnorm = 0;
   else
      gnorm = norm(G1,1);
   end
   hnorm = norm(h,1);
   pen   = f - beta'*G1 + gamma'*abs(h);
   rs    = delta*(d'*A*d + epsilon*xi*(gnorm + hnorm));
   doneGA = 0; it_ga  = - 1;
   while ~doneGA
      it_ga = it_ga + 1;
      alpha_l = alpha^it_ga;
      s = alpha_l*d; norms = norm(s); 
            % -- Maratos-Effect -----------------------
      if iter <= BeginMaratos
         x1 = x + s;
      else, % disp(' Maratos ')
         r  = maratos(F,x,d,y1,Parmeter);
         x1 = x + s - alpha_l^2*r;
      end
      f1 = feval(funfcn,x1,1,Parmeter);
      g1 = feval(funfcn,x1,2,Parmeter);
      G2 = min(g1,0);
      h1 = feval(funfcn,x1,3,Parmeter);
      h1norm  = norm(h1,1);
      if all(G2 >= - Eps),  g1norm = 0;
      else, g1norm = norm(G2,1);
      end
      pen1     = f1 - beta'*G2 + gamma'*abs(h1);
      pendiff1 = pen - pen1 - alpha_l*rs;
      done1    = (pendiff1 >= 0);
      done2    = 1;
      if flag
         pendiff2 = gnorm + hnorm - g1norm - h1norm ...
         - delta*alpha_l*xi*(gnorm + hnorm);
         done2 = pendiff2 >= 0;
      else
         done2 = (all(g1 > - tau) & all(abs(h1) < tau));
      end
      doneGA = (done1 & done2) | (it_ga > MaxitGA) | (norms < Tol);
   end
   if it_ga > MaxitGA
   %  disp(' Backtracking fails ');
   %  disp(' try to enlarge epsilon! ');
      errorcode = 2;
   end
   % new values  -------------------------------------
   gradf1 = feval(funfcn,x1,4,Parmeter);
   gradg1 = feval(funfcn,x1,5,Parmeter);
   gradh1 = feval(funfcn,x1,6,Parmeter);
   gradl1 = gradf1 - y'*gradg1 - z'*gradh1;
   % Acceleration methods 1/2/3 ------------------
   if beschl == 1, A = eye(n); end
   if beschl == 2
      s = x1 - x; v = gradl1 - gradl; w = A*s;
      n1 = s'*w; n2 = v*s;
      if (abs(n1) > Beps) & (abs(n2) > Beps)
         A = A - w*w'/n1 + v'*v/n2;
      end
      BFGS = BFGS + 1;
      if BFGS > n, BFGS = 1;  A = eye(n); end
   end
   if beschl == 3
      s = x1 - x; v = gradl1 - gradl; w = A*s;
      n1 = s'*w; n2 = v*s;
      if n2 >= 0.2*n1
         cc = 1;
      else
         cc = 0.8*n1/(n1 - n2);
      end
      v  = cc*v + (1 - cc)*w';
      n2 = v*s;
      if (abs(n1) > Beps) & (abs(n2) > Beps)
         A    = A - w*w'/n1 + v'*v/n2;
      else
         A = eye(n);
      end
      BFGS = BFGS + 1;
      if BFGS > n, BFGS = 1;  A = eye(n); end
   end
   normdiff = norm(x - x1); 
   eeps = 1E-9; 
   done5 = normdiff < eeps*(norm(x) + eeps); % STOP after Spell. p. 499
   x = x1; 
   if iter > Maxit, errorcode = 1; end;
   done = done5 | (errorcode ~= 0);
 %  done = done5 | (errorcode ~= 0) | norms < Tol;
   PERF_NORMS_ITQP_ITGA_ITER = [f,norms,it_qp,it_ga,iter]
end;
hnorm = norm(feval(funfcn,x,3,Parmeter))
