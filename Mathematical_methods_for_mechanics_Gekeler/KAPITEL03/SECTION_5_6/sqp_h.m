function [x1,f,errorcode] = sqp_h(funfcn,x,Parsqp,Parmeter)
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Method of sequental quadratic optimization
% Cf. Spellucci, \S 3.6
% f(x) = Min!, h(x) = 0
% FUNCTIONS:  dlqp_h.m
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
Tol = Parsqp(1); Maxit = Parsqp(2); epsilon = Parsqp(3);
beschl = Parsqp(4); 
MaxitGA = 30;
MaxitQP = 20;
Eps  = 1.0E-9;    % problem related
Beps = 1.0E-5;    % problem related
alpha = 0.5; delta = 0.1; eta = 0.05;
%----------------------------------------------------------
f0     = feval(funfcn,x,1,Parmeter);
gradf1 = feval(funfcn,x,4,Parmeter);
h0     = feval(funfcn,x,3,Parmeter);
gradh1 = feval(funfcn,x,6,Parmeter);
n = length(x); p = length(h0);
zero_n = zeros(n,1); zero_p = zeros(p,1); one_p = ones(p,1);
gamma = epsilon*one_p;
z     = gamma;
d     = zero_n;
tau   = max(max(abs(h0)),1);
A     = eye(n);
errorcode = 0; done = 0; iter = 0; zz = 1;
while ~done
   iter  = iter + 1;
   f     = feval(funfcn,x,1,Parmeter);
   h     = feval(funfcn,x,3,Parmeter);
   gradf = gradf1;
   gradh = gradh1;
   gradl = gradf - z'*gradh;
   %-- Data for quadratic optimization problem ----
   ecode = 0; xi = 10; it_qp = 0; doneQP    = 0;
   while ~doneQP
      xi = xi/10; it_qp = it_qp + 1;
      a = - gradf';
      C = gradh;
      c = xi*h;
      [d,z1,ff,ecode] = dlqp_h(A,a,C,c);
      doneQP = (~ecode) | (it_qp > MaxitQP);
      if it_qp > MaxitQP
        disp('Max. step number in QP-adaption ');
         ecode
         errorcode = 3
      end
   end;
   % -- Adaption of penalty vectors --------------
   gamma1 = abs(z1) + epsilon*one_p;
   theta  = 1;
   if (min(gamma1 - gamma) >= 0)
      theta = 0;
   end;
   pen0    = f0 + gamma1'*abs(h0);
   pen1    = f  + gamma1'*abs(h);
   pendiff = pen0 - pen1;
   if (theta == 1) & (pendiff >= zz*eta);
      zz    = zz + 1;
      gamma = gamma1;
   else
      K = find(abs(z1) + epsilon*one_p >= gamma);
      if length(K) > 0
         gamma(K) = abs(z1(K)) + 2*epsilon*ones(length(K),1);
      end;
   end;
   % -- Backtracking Strategy ----------------------
   flag  = (any(abs(h) > 0.5*tau));
   hnorm = norm(h,1);
   pen   = f  + gamma'*abs(h);
   rs    = delta*(d'*A*d + epsilon*xi*hnorm);
   doneGA = 0; it_ga = - 1;
   while ~doneGA
      it_ga  = it_ga + 1;
      alpha_l = alpha^it_ga;
      s      = alpha_l*d;
      norms  = norm(s);
      x1     = x + s;
      f1     = feval(funfcn,x1,1,Parmeter);
      h1     = feval(funfcn,x1,3,Parmeter);
      h1norm = norm(h1,1);
      pen1   = f1 + gamma'*abs(h1);
      pendiff1 = pen - pen1 - alpha_l*rs;
      done1 = (pendiff1 >= 0);
      done2 = 1;
      if flag
         pendiff2 = hnorm - h1norm - delta*alpha_l*xi*hnorm;
         done2 = (pendiff2 >= 0);
      else
         done2 = all(abs(h1) < tau);
      end;
      doneGA = (done1 & done2) | (it_ga > MaxitGA) | (norms < Tol);
   end;
   if it_ga > MaxitGA
      disp(' Backtracking fails');
      disp(' try by augmention of epsilon! ');
      errorcode = 2;
   end;
   % new values  -------------------------------
   gradf1 = feval(funfcn,x1,4,Parmeter);
   gradh1 = feval(funfcn,x1,6,Parmeter);
   gradl1 = gradf1 - z'*gradh1;
   x = x1;
   if iter > Maxit
   %   disp('Max. step number ');
      errorcode = 1;
   end;
   done = (norms < Tol) | (errorcode ~= 0);
   PERF_NORMS_ITQP_ITGA_ITER = [f,norms,it_qp,it_ga,iter]
%  beep
%  HNORM = norm(feval(funfcn,x,3,Parmeter))
end;
