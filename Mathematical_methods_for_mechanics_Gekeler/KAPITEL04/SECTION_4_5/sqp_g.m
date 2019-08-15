function [x1,f,errorcode,Pfad] = sqp_g(name,x,Tol,Maxit,Parmeter);
% GEKELER: OPTIMIERUNG
% Verfahren zur sequentiellen quadratischen Optimierung
% Vgl. Spellucci, \S 3.6
% f(x) = Min!, g(x) >= 0,
% FUNKTIONEN: dlqp_g.m
% INPUT  'name':   Name des Problems
%        x:        Beliebiger Punkt als Startvektor
%        Tol:      Toleranz
%        Maxit:    Maximale Schrittzahl
%        Parmeter: Zusaetzliche Parameter
% OUTPUT x Loesung bei Erfolg
%        f Wert der Zielfunktion
%        errorcode = 1: Max. Schrittzahl in Iteration
%        errorcode = 2: Max. Schrittzahl in Backtracking
%        errorcode = 3: Max. Schrittzahl in QP_Adaption
% Modifikationen:
% beschl = 1: A = I, Verfahren von ROSEN
% beschl = 2: Bei konvexen Problemen
% beschl = 3: Hilft ev. bei nichtkonvexen Problemen
% PARAMETER --------------
Tol     = Parsqp(1);
Maxit   = Parsqp(2);
epsilon = Parsqp(3);
beschl  = Parsqp(4);
F       = [name];
Pfad    = [x];
MaxitGA = 20;
MaxitQP = 20;
Eps     = 1.0E-10;    % problembezogen
Beps    = 1.0E-5;     % problembezogen
alpha   = 0.5;
delta   = 0.1;
eta     = 0.05;
%----------------------------------------------------------
f0      = feval(F,x,1,Parmeter);
gradf1  = feval(F,x,4,Parmeter);
g0      = feval(F,x,2,Parmeter);
gradg1  = feval(F,x,5,Parmeter);
n       = length(x);
m       = length(g0);
zero_m  = zeros(m,1);
zero_n  = zeros(n,1);
one_m   = ones(m,1);
G0      = min(g0,zero_m);
beta    = epsilon*one_m;
y       = beta;
d       = zero_n;

tau     = max(- min(g0), 1);
A       = eye(n);

errorcode = 0;
done    = 0;
iter    = 0;
zz      = 1;

while ~done
   iter   = iter + 1;
   f      = feval(F,x,1,Parmeter);
   g      = feval(F,x,2,Parmeter);
   G1     = min(g,zero_m);
   gradf  = gradf1;
   gradg  = gradg1;
   gradl  = gradf - y.'*gradg;
   % --- Werte fuer quadratische Optimierungsproblem --
   ecode  = 0;
   xi     = 10;
   it_qp  = 0;
   doneQP = 0;
   while ~doneQP
      xi     = xi/10;
      it_qp  = it_qp + 1;
      a      = - gradf.';
      IA     = find(g <= Eps);
      IB     = find(g > Eps);
      B      = [gradg(IA,:); gradg(IB,:)];
      b      = [xi*g(IA); g(IB)];
      [d,z,r,ecode] = dlqp_g(A,a,B,b);
      doneQP = (~ecode) | (it_qp > MaxitQP);
      if it_qp > MaxitQP
         errorcode = 3;
      end
   end;
   y  = zero_m;
   l1 = length(IA);
   l2 = length(IB);
   if l1 > 0
      y(IA) = z(1:l1);
   end;
   if l2 > 0
      y(IB) = z(l1+1:l1+l2);
   end;
   % -- Anpassen der Penalty-Vektoren -----------------
   beta1 = y + epsilon*one_m;
   theta = 1;
   if min(beta1 - beta) >= 0
      theta = 0;
   end;
   pen0    = f0 - beta1.'*G0;
   pen1    = f  - beta1.'*G1;
   pendiff = pen0 - pen1;
   if (theta == 1) & (pendiff >= zz*eta) & (zz >= length(x));
      zz   = 1;
      beta = beta1;
   else, disp(' Alter weights ')
      zz = zz+1;
      K    = find(y + epsilon*one_m >= beta);
      if length(K) > 0
         beta(K) = y(K) + 2*epsilon*ones(length(K),1);
      end;
   end;
   % -- Backtracking-Strategie ------------------------
   flag = (any(g < - 0.5*tau));
   if all(G1 >= - Eps)
      gnorm = 0;
   else
      gnorm = norm(G1,1);
   end;
   pen  = f - beta.'*G1;               % Penalty-Funktion
   rs   = delta*(d.'*A*d + epsilon*xi*gnorm);
   doneGA = 0;
   it_ga   = - 1;
   while ~doneGA
      it_ga   = it_ga + 1;
      alpha_l = alpha^it_ga;
      s       = alpha_l*d;
      norms   = norm(s);
      x1      = x + s;
      f1      = feval(F,x1,1,Parmeter);
      g1      = feval(F,x1,2,Parmeter);
      G2      = min(g1,0);
      if all(G2 >= - Eps)
         g1norm = 0;
      else
         g1norm = norm(G2,1);
      end;
      pen1  = f1 - beta.'*G2;     % modif. Penaltyfunktion
      pendiff = pen - pen1;
      done1   = (pendiff >= alpha_l*rs);
      done2   = 1;
      if flag
         done2 = (gnorm - g1norm >= delta*alpha_l*xi*gnorm);
      else
         done2 = (all(g1 > - tau)); %kann man weglassen
      end;
      doneGA = (done1 & done2) | (it_ga > MaxitGA) | (norms < Tol);
   end;
   if it_ga > MaxitGA
%     disp(' Backtracking versagt');
%     disp(' ev. epsilon vergroessern! ');
      errorcode = 2;
   end;
   % neue Werte  -------------------------------------
   gradf1 = feval(F,x1,4,Parmeter);
   gradg1 = feval(F,x1,5,Parmeter);
   gradl1 = gradf1 - y.'*gradg1;

   % Beschleunigungs-Verfahren 1/2/3------------------
   if beschl == 1
      A       = eye(n);
   end;
   if beschl == 2
      v       = gradl1 - gradl;
      w       = A*s;
      n1      = s.'*w;
      n2      = v*s;
      if (abs(n1) > Beps) & (abs(n2) > Beps)
         A    = A - w*w.'/n1 + v.'*v/n2;
      end;
   end;
   if beschl == 3
      v       = gradl1 - gradl;
      w       = A*s;
      n1      = s.'*w;
      n2      = v*s;
      if n2 >= 0.2*n1
         cc   = 1;
      else
         cc   = 0.8*n1/(n1 - n2);
      end;
      v       = cc*v + (1 - cc)*w';
      n2      = v*s;
      if (abs(n1) > Beps) & (abs(n2) > Beps)
         A    = A - w*w.'/n1 + v.'*v/n2;
      else
         A    = eye(n);
      end;
   end;
   x          = x1;
   if iter >= Maxit
      errorcode = 1;
   end;
   done       = (norms < Tol) | (errorcode ~= 0);
   PERF_NORMS_ITQP_ITGA_ITER = [f,norms,it_qp,it_ga,iter]
   beep
end;
