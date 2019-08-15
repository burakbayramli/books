function [tt,uout,vout,wout,errorcode] ...
         = hem4(fcn,t,t_end,u,v,w,parmtr2,parmtr3);
% halb-explizite Runge-Kutta-Verfahren nach Brasey
% Input parameters
% fcn	  Name des mechanischen Systems (extern)
%         F(flag,T,X,Y,Z,Parmeter);
% t       Anfangswert fuer Zeit t
% t_end   Endwert fuer Zeit t (t_end - t positiv oder negativ (??))
% u       Anfangswert fuer u
% v       Anfangswert fuer v
% Output parameters
% uout    Loesung u an t-Stellen
% vout    Loesung v an t-Stellen
% wout    Loesung w an t-Stellen
% -- Initial Preparations----------------------------
EPS     = parmtr2(1); % Local Tolerance
tau_max = parmtr2(2); % Maximal stepsize
tau     = parmtr2(3); % Initial stepsize guess
hem     = parmtr2(4);  % HEM3.M oder HEM4.M
nmax    = 1000; % maximal number of steps
uround  = 5.0E-8; % uround  smallest nr. with 1.0 + UROUND > 1.0
                 % to be adapted by the user
nstep  = 0;  % Number of computed steps
errorcode = 0;
posneg = sign(t_end - t);
tau    = posneg*min(abs(tau_max),max(abs(tau),1.0E-4));
EPS    = max(7.0*uround,EPS);
reject = 0;
uout = u; vout = v; wout = w;
tt     = t;

STEPWIDTH = tau;
% ---Basic integration step--------------------------------
while (nstep <= nmax) & (t + 0.1*tau ~= t) & ...
       ((t - t_end)*posneg + uround <= 0)
   if ((t + tau - t_end)*posneg > 0.0)
      tau = t_end - t;
   end;
   nstep = nstep + 1;
   % ------------------------------------------------------------
   if hem == 3
      [un,vn,wn,u_aux] = hem3_kern(fcn,t,tau,u,v,w,parmtr2,parmtr3);
   end
   if hem == 4
      [un,vn,wn,u_aux] = hem4_kern(fcn,t,tau,u,v,w,parmtr2,parmtr3);
       %[un,vn,wn,u_aux] = hem4_kern_s(fcn,t,tau,u,v,w,parmtr2,parmtr3);
   end
   % ------------------------------------------------------------
   tph = t + tau;
   % ---Error estimation-------------------------------------
   denom = max(max(max(abs(u)),max(abs(un))),1.0E-5);
   denom = max(2.0*uround/EPS,denom);
   err   = norm(un - u_aux)/(sqrt(length(u))*denom(1));
   % ---Computation of tau_n ----------------------------------
   % ---We require that 0.2 <= tau_n/tau <= 10.0----------------
   facmin = 0.2; facmax = 10.0; fac = 0.9;
   if hem == 3
      fac = max(0.1,min(5,sqrt(err/EPS)/0.9));
   end
   if hem == 4
      fac = max(0.1,min(5,(err/EPS)^0.33/0.9));
   end
   tau_n = tau/fac;
   if err <= EPS
      u = un; v = vn; wn = w;
      t  = tph;
      if (abs(tau_n) > tau_max) tau_n = posneg*tau_max; end
      CC = min(abs(tau_n),abs(tau));
      if (reject == 1) tau_n = posneg*CC; end
      reject = 0;
   else
      reject = 1; % Step is rejected
   end;
   tau = tau_n;
   uout = [uout, un]; vout = [vout, vn]; wout = [wout, wn];
   tt   = [tt, t];
   nstep_tau_t = [nstep, tau,t]  % Monitor
   %STEPWIDTH = [STEPWIDTH; tau];
end;
if nstep > nmax
   disp('max. step number reached in hem4.m')
   errorcode = 1;
end
if (t + 0.1*tau) == t
   disp('min. step size reached in hem4.m')
   errorcode = 2;
end
%STEPWIDTH
