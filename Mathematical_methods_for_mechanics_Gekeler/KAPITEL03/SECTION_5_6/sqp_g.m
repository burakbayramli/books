function [x1,f,errorcode,Pfad] = sqp_g(funfcn,x,Parsqp,Parmeter)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Method of sequential quadratic optimization
% Cf. Spellucci, \S 3.6
% f(x) = Min!, g(x) >= 0,
% FUNCTIONS: dlqp_g.m
% INPUT  funfcn: Name of the problem
%        x:      Arbitrary point as start vector
%        Tol:    Tolerance
%        Maxit:  Maximal step number
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
% PARAMETER ----------
Tol = Parsqp(1); Maxit = Parsqp(2); epsilon = Parsqp(3);
beschl = Parsqp(4); BeginMaratos = Parsqp(5);
Pfad = [x]; MaxitGA = 50; MaxitQP = 20;
Eps  = 1.0E-5;    % problem related
Beps = 1.0E-7;    % problem related
alpha = 0.5; delta = 0.1; eta = 0.05; BFGS = 1; 
%----------------------------------------------------------
f0     = feval(funfcn,x,1,Parmeter);
gradf1 = feval(funfcn,x,4,Parmeter);
g0     = feval(funfcn,x,2,Parmeter);
gradg1 = feval(funfcn,x,5,Parmeter);
n = length(x); m = length(g0);
zero_m = zeros(m,1); zero_n = zeros(n,1); one_m = ones(m,1);
G0 = min(g0,zero_m); beta = epsilon*one_m; y = beta;
d = zero_n; tau = max(- min(g0), 1); A = eye(n);
errorcode = 0; done = 0; iter = 0; zz = 1;
while ~done
   iter = iter + 1;
   f  = feval(funfcn,x,1,Parmeter);
   g  = feval(funfcn,x,2,Parmeter);
   G1 = min(g,zero_m);
   gradf = gradf1; gradg = gradg1;
   gradl  = gradf - y.'*gradg;
   % --- Data for quadratic optimization problem --
   ecode = 0; xi = 10; it_qp = 0; doneQP = 0;
   while ~doneQP
      xi = xi/10;
      it_qp = it_qp + 1;  
      a  = - gradf.';
      IA = find(g <= Eps); IB = find(g > Eps);
      % error in Spell. p. 495, see p. 488
      B  = [gradg(IA,:); gradg(IB,:)];
      b  = [xi*g(IA); g(IB)];
      [d,z,r,ecode] = dlqp_g(A,a,B,b);
      doneQP = (~ecode) | (it_qp > MaxitQP);
      if it_qp > MaxitQP, errorcode = 3; end
   end;
   y = zero_m; l1 = length(IA); l2 = length(IB);
   if l1 > 0
      y(IA) = z(1:l1);
   end;
   if l2 > 0
      y(IB) = z(l1+1:l1+l2);
   end;
   % -- Adaption of penalty vectors -----------------
   beta1 = y + epsilon*one_m;
   theta = 1;
   if min(beta1 - beta) >= 0
      theta = 0;
   end;
   pen0 = f0 - beta1.'*G0;
   pen1 = f  - beta1.'*G1;
   pendiff = pen0 - pen1;
   if (theta == 1) & (pendiff >= zz*eta)& (zz > length(x));
      zz = 1; beta = beta1;
   else
      zz = zz + 1;
      K  = find(y + epsilon*one_m >= beta);
      if length(K) > 0
         beta(K) = y(K) + 2*epsilon*ones(length(K),1);
      end;
   end;
   % -- Backtracking Strategy ------------------------
   flag = (any(g < - 0.5*tau));
   if all(G1 >= - Eps)
      gnorm = 0;
   else
      gnorm = norm(G1,1);
   end;
   pen = f - beta.'*G1;               % Penalty function
   rs  = delta*(d.'*A*d + epsilon*xi*gnorm);
   doneGA = 0; it_ga = - 1;
   while ~doneGA
      it_ga   = it_ga + 1;
      alpha_l = alpha^it_ga;
      s       = alpha_l*d;
      norms   = norm(s);
      % -- Maratos-Effect -----------------------
      if iter <= BeginMaratos
         x1 = x + s;
      else, % disp(' Maratos ')
         r  = maratos_g(funfcn,x,d,y,Parmeter);
         x1 = x + s - alpha_l^2*r;
      end
      f1 = feval(funfcn,x1,1,Parmeter);
      g1 = feval(funfcn,x1,2,Parmeter);
      G2 = min(g1,0);
      if all(G2 >= - Eps)
         g1norm = 0;
      else
         g1norm = norm(G2,1);
      end;
      pen1  = f1 - beta.'*G2;     % modif. penalty function
      pendiff = pen - pen1;
      done1 = (pendiff >= alpha_l*rs);
      done2 = 1;
      if flag
         done2 = (gnorm - g1norm >= delta*alpha_l*xi*gnorm);
      else
         done2 = (all(g1 > - tau)); % may be omitted
      end;
      doneGA = (done1 & done2) | (it_ga > MaxitGA) | (norms < Tol);
   end;
   if it_ga > MaxitGA
%     disp(' Backtracking fails ');
%     disp(' try enlargement of epsilon! ');
      errorcode = 2;
   end;
   % -- New values ------------------------------------
   gradf1 = feval(funfcn,x1,4,Parmeter);
   gradg1 = feval(funfcn,x1,5,Parmeter);
   gradl1 = gradf1 - y.'*gradg1;
   % Acceleration procedures 1/2/3 ------------------
   if beschl == 1, A = eye(n); end;
   if beschl == 2
      s = x1 - x; v  = gradl1 - gradl; w  = A*s;
      n1 = s.'*w; n2 = v*s;
      if (abs(n1) > Beps) & (abs(n2) > Beps)
         %disp(' Korrektur ')
         A = A - w*w.'/n1 + v.'*v/n2;
      end;
      BFGS = BFGS + 1;
      if BFGS > n, BFGS = 1;  A = eye(n); end
   end;
   if beschl == 3
      s = x1 - x; v  = gradl1 - gradl; w  = A*s;
      n1 = s.'*w; n2 = v*s;
      if n2 >= 0.2*n1
         cc = 1;
      else
         cc = 0.8*n1/(n1 - n2);
      end;
      v  = cc*v + (1 - cc)*w.';
      n2 = v*s;
      if (abs(n1) > Beps) & (abs(n2) > Beps)
         A = A - w*w.'/n1 + v.'*v/n2;
      else
         A = eye(n);
      end;
      BFGS = BFGS + 1;
      if BFGS > n, BFGS = 1;  A = eye(n); end
   end;
   normdiff = norm(x - x1); 
   eeps = 1E-9; 
   done5 = normdiff < eeps*(norm(x) + eeps); % STOP after Spell. p. 499
   x = x1; Pfad = [Pfad,x];
   if iter > Maxit, errorcode = 1; end;
   done = done5 | (errorcode ~= 0);
 %  done = done5 | (errorcode ~= 0) | norms < Tol;
   PERF_NORMS_ITQP_ITGA_ITER = [f,norms,it_qp,it_ga,iter]
end;
   