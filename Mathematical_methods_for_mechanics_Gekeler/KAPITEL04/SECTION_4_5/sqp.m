function [x1,f,errorcode] = sqp(F,x,Parsqp,Parmeter);
% GEKELER: Math. Meth. Mechanics
% Sequential Quadratic Programing
% Cf. Spellucci, \S 3.6
% f(x) = Min!, g(x) >= 0, h(x) = 0
% FUNCTIONS: dlqp.m, maratos.m
% INPUT  'name':   Name of problem
%        x:        Arbitrary initial vector
%        Tol:      Tolerance
%        Maxit:    Maximal  step number
%        Parmeter: Additional parameter
% OUTPUT x solution if success
%        f: Value of objective function
%        errorcode = 1: Max. step number in iteration
%        errorcode = 2: Max. step number in backtracking
%        errorcode = 3: Max. step number in QP_adaption
% Modifications:
% acceleration = 1: A = I, method of  ROSEN
% acceleration = 2: BFGS method, for convex problems
% acceleration = 3: may help in non-convex problems
% PARAMETER ----------
Tol = Parsqp(1); Maxit = Parsqp(2); epsilon = Parsqp(3); 
acceleration = Parsqp(4); BeginMaratos = Parsqp(5);
MaxitGA = 20; MaxitQP = 20; alpha = 0.5; delta = 0.1; eta = 0.05;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delta = 0.01; eta = 0.01; %besser ?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Eps  = 1.0E-10;    %problem related
Beps = 1.0E-5;     %problem related
%----------------------------------------------------------
f0 = feval(F,x,1,Parmeter); g0 = feval(F,x,2,Parmeter);
h0 = feval(F,x,3,Parmeter);
gradf1 = feval(F,x,4,Parmeter); gradg1 = feval(F,x,5,Parmeter);
gradh1 = feval(F,x,6,Parmeter);
n = length(x); m = length(g0); p = length(h0);

G0 = min(g0,0); beta = epsilon*ones(m,1); gamma = epsilon*ones(p,1);
y = beta; z = gamma; d = zeros(n,1);

tau1 = max(- min(g0),1); tau2 = max(max(abs(h0)),1);
tau  = max(tau1,tau2);
A = eye(n); errorcode = 0; done = 0; iter = 0; zz = 1;
while ~done
   iter = iter + 1;
   if iter == n, A = eye(n); end %Aenderung 21.08.06
   f = feval(F,x,1,Parmeter); g = feval(F,x,2,Parmeter);
   h = feval(F,x,3,Parmeter);
   gradf = gradf1; gradg = gradg1; gradh = gradh1;
   gradl = gradf - y.'*gradg - z.'*gradh;
   G1    = min(g,0);
   % -- Data for Quadratic Programming Problem --
   ecode = 0; xi = 10; it_qp = 0; doneQP = 0;
   while ~doneQP
      xi = xi/10; it_qp  = it_qp + 1; a = - gradf.';
      IA = find(g <=- Eps); IB = find(g > - Eps); %%Aenderung
      B  = [gradg(IA,:); gradg(IB,:)];
      b  = [xi*g(IA); g(IB)];
      C  = gradh; c  = xi*h;
      [d,v,z1,ff,ecode] = dlqp(A,a,B,b,C,c);
      %optimset('largescale','off');
      %[x,f,exitflag,output,lambda] = quadprog(A,-a,-B,b,C,c);
      %exitflag
      %pause
      %ecode = exitflag <= 0;
      %v = lambda.ineqlin; z1 = lambda.eqlin;
      doneQP = (~ecode) | (it_qp > MaxitQP);
      if it_qp > MaxitQP
      %  disp('Max. step number in QP-adaption attained ');
         ecode, errorcode = 3;
      end
   end;
   y1 = zeros(m,1); l1 = length(IA); l2 = length(IB);
   if l1 > 0, y1(IA) = v(1:l1); end
   if l2 > 0, y1(IB) = v(l1+1:l1+l2); end
   % -- Adaption of Penalty Vectors -------------
   METHOD = 1; % Nach Spellucci und nach Powell (schlecht)
   switch METHOD
   case 1
      beta1 = y1 + epsilon*ones(m,1); gamma1 = abs(z1) + epsilon*ones(p,1);
      theta = 1;
      if (min(beta1 - beta) >= 0) & (min(gamma1 - gamma) >= 0)
         theta  = 0;
      end;
      penalty0    = f0 - beta1.'*G0 + gamma1.'*abs(h0);
      penalty1    = f  - beta1.'*G1 + gamma1.'*abs(h);
      pendiff = penalty0 - penalty1;
      if (theta == 1) & (pendiff >= zz*eta);
         zz = zz + 1; beta = beta1; gamma  = gamma1;
      else
         disp(' Alter Weights ')
         K = find(y1 + epsilon >= beta);
         if length(K) > 0
            beta(K) = y1(K) + 2*epsilon*ones(length(K),1);
         end;
         K = find(abs(z1) + epsilon*ones(p,1) >= gamma);
         if length(K) > 0
            gamma(K) = abs(z1(K)) + 2*epsilon*ones(length(K),1);
         end;
      end;
   case 2 % nach POWELL
      for K = 1:m
         beta(K) = max(y1(K),0.5*(beta(K) + y1(K)));
      end
      for K = 1:p
         gamma(K) = max(abs(z1(K)),0.5*(gamma(K) + abs(z1(K))));
      end
   end
   % -- Backtracking-Strategy -------------------
   flag1 = (any(g < - 0.5*tau)); flag2 = (any(abs(h) > 0.5*tau));
   flag      = flag1 | flag2;
   if all(G1 >= - Eps), gnorm = 0;
   else,                gnorm = norm(G1,1);
   end;
   hnorm = norm(h,1);
   penalty   = f - beta.'*G1 + gamma.'*abs(h);
   rs    = delta*(d.'*A*d + epsilon*xi*(gnorm + hnorm));
   doneGA = 0; it_ga = - 1;
   while ~doneGA
      it_ga   = it_ga + 1;
      alpha_l = alpha^it_ga;
      s       = alpha_l*d;
      norms   = norm(s);   % ----------
      % -- Maratos-Effect -----------------------
      if iter <= BeginMaratos
         x1 = x + s;
      else, % disp(' Maratos ')
         r = maratos(F,x,d,y1,Parmeter);
         x1     = x + s - alpha_l^2*r;
      end
      f1 = feval(F,x1,1,Parmeter); g1 = feval(F,x1,2,Parmeter);
      h1 = feval(F,x1,3,Parmeter);
      G2 = min(g1,0);
      h1norm  = norm(h1,1);
      if all(G2 >= - Eps), g1norm = 0;
      else,                g1norm = norm(G2,1);
      end;
      penalty1     = f1 - beta.'*G2 + gamma.'*abs(h1);
      pendiff1 = penalty - penalty1 - alpha_l*rs;
      done1    = (pendiff1 >= 0);
      done2    = 1;
      if flag
         pendiff2 = gnorm + hnorm - g1norm - h1norm ...
         - delta*alpha_l*xi*(gnorm + hnorm);
         done2 = pendiff2 >= 0;
      else
         done2 = (all(g1 > - tau) & all(abs(h1) < tau));
      end;
      doneGA = (done1 & done2) | (it_ga > MaxitGA) | (norms < Tol);
   end;
   if it_ga > MaxitGA, %disp(' Backtracking fails ');
      %  disp(' ev. epsilon vergroessern! '); 
      errorcode = 2;
   end;
   % -- New Values for gradients ----------------------
   gradf1 = feval(F,x1,4,Parmeter); gradg1 = feval(F,x1,5,Parmeter);
   gradh1 = feval(F,x1,6,Parmeter);
   gradl1 = gradf1 - y.'*gradg1 - z.'*gradh1;
   gradl  = gradf - y.'*gradg - z.'*gradh;
   % -- Acceleration Modifications 1/2/3 -------
   switch acceleration
   case 1, A = eye(n);
   case 2, Z = (gradl1 - gradl).';
       D = x1 - x;
       W = A*D;  NENNER1 = D.'*W;  NENNER2 = Z.'*D;
      if (abs(NENNER1) > Beps) & (abs(NENNER2) > Beps)
         A = A - W*W.'/NENNER1 + Z*Z./NENNER2;
      end   
   case 3, Z = (gradl1 - gradl).'; 
      D = x1 - x;
      W  = A*D; NENNER1 = D.'*W; AUX = Z.'*D;
      if AUX >= 0.2*NENNER1, cc = 1;
      else,            cc = 0.8*NENNER1/(NENNER1 - AUX);
      end
      Y  = cc*Z + (1 - cc)*W; NENNER2 = Y.'*D;
      if (abs(NENNER1) > Beps) & (abs(NENNER2) > Beps)
         A  = A - W*W.'/NENNER1 + Y*Y.'/NENNER2;
      end;
   end;
   x = x1; y = y1; z = z1;
   if iter >= Maxit, %  disp('Max. step number attained');
      errorcode = 1;
   end;
   done = (norms < Tol) | (errorcode ~= 0);
   PERF_NORMS_ITQP_ITGA_ITER = [f,norms,it_qp,it_ga,iter]
 % beep
end;
%HH = feval(F,x,3,Parmeter);
%hnorm = max(abs(HH))
