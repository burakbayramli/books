function [x1,f,errorcode] = sqp_h_a(name,x,Parsqp,Pardlqp,Parmeter);
% Verfahren zur sequentiellen quadratischen Optimierung
% Vgl. Spellucci, \S 3.6
% f(x) = Min!, h(x) = 0
% FUNKTIONEN:  dlqp_h.m
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
% PARAMETER ----------
Tol    = Parsqp(1); Maxit = Parsqp(2); epsilon = Parsqp(3);
beschl = Parsqp(4);
F       = [name];
MaxitGA = 40;
MaxitQP = 30;
alpha   = 0.5;
delta   = 0.1;
eta     = 0.05;
%----------------------------------------------------------
f0           = feval(F,x,1,Parmeter);
gradf1       = feval(F,x,4,Parmeter);
h0           = feval(F,x,3,Parmeter);
gradh1       = feval(F,x,6,Parmeter);
n            = length(x);
p            = length(h0);
zero_n       = zeros(n,1);
zero_p       = zeros(p,1);
one_p        = ones(p,1);

gamma        = epsilon*one_p;
z            = gamma;
d            = zero_n;
tau          = max(max(abs(h0)),1);
A            = eye(n);

errorcode    = 0;
done         = 0;
iter         = 0;
zz           = 1;

while ~done
   iter      = iter + 1;
   f         = feval(F,x,1,Parmeter);
   h         = feval(F,x,3,Parmeter);
   gradf     = gradf1;
   gradh     = gradh1;
   gradl     = gradf - z'*gradh;
   %-- Werte fuer quadratische Optimierungsproblem ----
   ecode     = 0;
   xi        = 10;
   it_qp     = 0;
   doneQP    = 0;
   while ~doneQP
      xi     = xi/10;
      it_qp  = it_qp + 1;
      a      = - gradf';
      C      = gradh;
      c      = xi*h;
      [d,z1,ff,ecode] = dlqp_h(A,a,C,c,Pardlqp);
      doneQP = (~ecode) | (it_qp > MaxitQP);
      if it_qp > MaxitQP
       % disp('Max. Schrittzahl in QP-Adaption erreicht ');
         ecode
         errorcode = 3;
      end
   end;
   % -- Anpassen der Penalty-Vektoren ----------------
   gamma1    = abs(z1) + epsilon*one_p;
   theta     = 1;
   if (min(gamma1 - gamma) >= 0)
      theta  = 0;
   end;
   pen0      = f0 + gamma1'*abs(h0);
   pen1      = f  + gamma1'*abs(h);
   pendiff   = pen0 - pen1;
   if (theta == 1) & (pendiff >= zz*eta);
      zz     = zz + 1;
      gamma  = gamma1;
   else
      K      = find(abs(z1) + epsilon*one_p >= gamma);
      if length(K) > 0
         gamma(K) = abs(z1(K)) + 2*epsilon*ones(length(K),1);
      end;
   end;
   % -- Backtracking-Strategie ------
   flag      = (any(abs(h) > 0.5*tau));
   hnorm     = norm(h,1);
   pen       = f  + gamma'*abs(h);
   rs        = delta*(d'*A*d + epsilon*xi*hnorm);
   doneGA    = 0;
   it_ga     = - 1;
   while ~doneGA
      it_ga  = it_ga + 1;
      alpha_l = alpha^it_ga;
      s      = alpha_l*d;
      norms  = norm(s);
      x1     = x + s;
      %%%%%%%%%%%%%%%%%%%%%%%%%%
      n = Parmeter(1);
      x1(3*n+4:4*(n+1)) = mod(x1(3*n+4:4*(n+1)),2*pi);
      x1(4*n+5:5*(n+1)) = mod(x1(4*n+5:5*(n+1)),2*pi);

      %%%%%%%%%%%%%%%%%%%%%%%%%%
      f1     = feval(F,x1,1,Parmeter);
      h1     = feval(F,x1,3,Parmeter);
      h1norm = norm(h1,1);
      pen1   = f1 + gamma'*abs(h1);
      pendiff1 = pen - pen1 - alpha_l*rs;
      done1  = (pendiff1 >= 0);
      done2  = 1;
      if flag
         pendiff2 = hnorm - h1norm - delta*alpha_l*xi*hnorm;
         done2    = (pendiff2 >= 0);
      else
         done2    = all(abs(h1) < tau);
      end;
      doneGA = (done1 & done2) | (it_ga > MaxitGA) | (norms < Tol);
   end;
   if it_ga > MaxitGA
     % disp(' Backtracking versagt');
     % disp(' ev. epsilon vergroessern! ');
      errorcode = 2;
   end;
   % neue Werte  -------------------------------------
   gradf1    = feval(F,x1,4,Parmeter);
   gradh1    = feval(F,x1,6,Parmeter);
   gradl1    = gradf1 - z'*gradh1;
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   x         = x1;
   n = Parmeter(1);
   x(3*n+4:4*(n+1)) = mod(x(3*n+4:4*(n+1)),2*pi);
   x(4*n+5:5*(n+1)) = mod(x(4*n+5:5*(n+1)),2*pi);
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if iter >= Maxit, errorcode = 1; end;
   done = (norms < Tol) | (errorcode ~= 0);
 %  beep
   HH = (feval(F,x,3,Parmeter));
   normh = norm(HH,inf);
   NORMH_NORMS_ITQP_ITGA_ITER = [normh,norms,it_qp,it_ga,iter]
end;
  %clf, plot(HH), grid on
  %pause
