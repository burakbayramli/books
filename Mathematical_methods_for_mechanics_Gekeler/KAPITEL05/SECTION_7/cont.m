function [WEG,errorcode] = cont(F,X,Parcont,Parmeter)
% Continuation after Allgower and Georg
n1 = length(X); n = n1 - 1; errorcode = 0;
% -- Parameter -------------------------
maxjac = Parcont(1); tol = Parcont(2); Richtung = Parcont(3);
ctmax  = 0.6; dmax = 0.4; hmax = 1.0; fmax = 2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
ctmax = 0.6; dmax = 0.4; hmax = 0.002;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hmin   = 1.0E-5; cdmax = 1.0E3;
h      = 0.03;   eta   = 0.1;
WEG    = [];
newt = 0; mapct = 0; jacct = 0;
B      = feval(F,X,2,Parmeter)'; % B = (n1,n)-Matrix
jacct  = jacct + 1;
[Q,R]  = qr(B);
Cond   = cond(R);
Y0     = feval(F,X,1,Parmeter);
if abs(Y0) > tol, errorcode = 1; return; end
if Cond > cdmax,  errorcode = 2; return; end;
t = Q(:,n1);
if t(n1) > 0, ori = 1; else ori = - 1; end
succ = 0; pcit = 0;
% --- begin PC method -------------------------------------
while (abs(h) >= hmin) & (jacct <= maxjac) & (succ == 0)
   pcit = pcit + 1;
   WEG  = [WEG, X];
   u    = X + h*ori*t;
   fac  = 1/fmax;
   B    = feval(F,X,2,Parmeter)';
   jacct = jacct + 1;
   %--------------------------------------
   [Q,R] = qr(B);
   Cond  = cond(R);
   iter  = 0; dist  = 2*tol; newtonflag = 1;
   while (dist >= tol) & (newtonflag == 1) & (Cond <= cdmax)
      iter     = iter + 1;
      y        = feval(F,u,1,Parmeter);
      mapct    = mapct + 1;
      [u,dist] = newtonstep(Q,R,u,y,n,n1);
      if dist > dmax
         newtonflag = 0;
      else
         fac = max(fac,sqrt(dist/dmax)*fmax);
         if iter >= 2
            contr = dist/(disto + tol*eta);
            if contr > ctmax
               newtonflag = 0;
            else
               fac = max(fac,sqrt(contr/ctmax)*fmax);
            end;
         end;
         disto = dist;
      end;
   end;
   if newtonflag == 0, h = h/fmax; end;
   if dist < tol
      % espezially for Example 5 (Allgower)
      % not in closed pathes !!
      %succ      = 0;
      %if u(n1) >= 1, newt = 1; end;
      %if newt == 1
      %   h   = - (u(n1)- 1)/(ori*Q(n1,n1));
      %   if abs(h) < hmin, succ = 1; end;
      %else
         fac = min(fac,fmax);
         h   = min(abs(h)/fac,hmax);
      end;
      X = u;
      t = Q(:,n1);
      s = 1;
      for k = 1:n
          s = s*R(k,k);
      end;
      % Regard reflections -----------
      sgn = 1;
      if rem(n,2) == 1 sgn = - 1; end
      ori = Richtung*sgn*sign(s);
      %ori = -sgn*sign(s); %Beispiel 3
   end;
%end;
%--- end PC method ----------------------------
if (abs(h) < hmin) & (succ == 0) errorcode = 3; end
if (jacct > maxjac) & (succ == 0)errorcode = 4; end
