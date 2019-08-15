function [xout,errorcode] = dopri(fcn,t,x,t_end,parmtr2,parmtr3);
% Hairer et al.: Solving Ordinary Diff. Equations I
% Input parameters
% fcn	  Name (external) of procedure computing the first derivative
%	  F(X,Y,parmtr3)
% t       Initial t-value
% t_end   Final t-value (t_end - t positive or negative)
% x       Initial value for x
% Output parameters
% xout    matrix of solution vectors with suff. large difference
% -- Initial Preparations----------------------------
EPS     = parmtr2(1); % Local Tolerance
tau_max = parmtr2(2); % Maximal stepsize
tau     = parmtr2(3); % Initial stepsize guess
nmax    = 1000; % maximal number of steps
uround  = 5.0E-8; % uround  smallest nr. with 1.0 + UROUND > 1.0
                 % to be adapted by the user
nstep  = 0;  % Number of computed steps
errorcode = 0;
x1     = x;
[N,m]  = size(x);
posneg = sign(t_end - t);
tau    = posneg*min(abs(tau_max),max(abs(tau),1.0E-4));
EPS    = max(7.0*uround,EPS);
reject = 0;
xout   = x;
K1     = feval(fcn,t,x,parmtr3);
% ---Basic integration step------------------------------------------
while (nstep <= nmax) & (t + 0.1*tau ~= t) & ((t - t_end)*posneg + uround <= 0)
   if ((t + tau - t_end)*posneg > 0.0)
      tau = t_end - t;
   end;
   nstep = nstep + 1;
   % ---The first 6 stages----------------------------------------------
   x1 = x + tau*0.2*K1;
   tt = t + 0.2*tau;
   K2 = feval(fcn,tt,x1,parmtr3);

   x1 = x + tau*((3/40)*K1 + (9/40)*K2);
   tt = t + 0.3*tau;
   K3 = feval(fcn,tt,x1,parmtr3);

   x1 = x + tau*((44/45)*K1 - (56/15)*K2 + (32/9)*K3);
   tt = t + 0.8*tau;
   K4 = feval(fcn,tt,x1,parmtr3);

   x1 = x + tau*((19372/6561)*K1 - (25360/2187)*K2  ...
       + (64448/6561)*K3 - (212/729)* K4);
   tt = t + (8/9)*tau;
   K5 = feval(fcn,tt,x1,parmtr3);

   x1  = x + tau*((9017/3168)*K1 - (355/33)*K2);
   x1  = x1 + tau*((46732/5247)*K3 + (49/176)*K4 - (5103/18656)*K5);
   tph = t + tau;
   K2  = feval(fcn,tph,x1,parmtr3);

   x1 = x + tau*((35/384)*K1 + (500/1113)*K3);
   x1 = x1 + tau*((125/192)*K4 - (2187/6784)*K5 + (11/84)*K2);
   % ---compute intermediate sum to save memory--------------
   K2 = (71/57600)*K1 - (71/16695)*K3 ...
   + (71/1920)*K4 - (17253/339200)*K5 + (22/525)*K2;
   % --- the last stage--------------------------------------
   K3   = feval(fcn,tph,x1,parmtr3);
   K4   = (K2 - (1/40)*K3)*tau;
   % ---Error estimation-------------------------------------
   denom = max(max(max(abs(x)),max(abs(x1))),1.0E-5);
   denom = max(2.0*uround/EPS,denom);
   err   = K4'*K4/denom(1)^2;
   err   = sqrt(err/N);
   % ---Computation of tau_n ----------------------------------
   % ---We require that 0.2 <= tau_n/tau <= 10.0----------------
   facmin = 0.2; facmax = 10.0; fac = 0.9;
   fac = max(0.1,min(5,(err/EPS)^0.2/0.9));
   tau_n = tau/fac;
   if err <= EPS
      % --- STEP is accepted------------------------------------
      % by reason of storage place not all intermediate results --
      % are storaged ---------------------------------------------
      z = max(abs(x - x1));
      if (z > 0.05) xout = [xout, x1]; end
      K1 = K3; x = x1;
      t  = tph;
      if (abs(tau_n) > tau_max) tau_n = posneg*tau_max; end
      CC = min(abs(tau_n),abs(tau));
      if (reject == 1) tau_n = posneg*CC; end
      reject = 0;
   else
      reject = 1; % Step is rejected
   end;
   tau = tau_n;
end;
if nstep > nmax
   disp('max. step number reached in dopri.m')
   errorcode = 1;
end
if (t + 0.1*tau) == t
   disp('min. step size reached in dopri.m')
   errorcode = 2;
end
