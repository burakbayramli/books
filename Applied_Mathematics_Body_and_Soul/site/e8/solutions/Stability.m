function [Sc, Ts] = Stability(U, t, f, T, Phi0)

% Computes the stability factor, Sc,  to the problem
%  u' (t) = f(t,u), u(0) = u0
% 
% by computing the dual problem
% - phi' = J^t phi(t) ,  phi(T) = phi0
%
% or really the problem
% psi(t) = J^t (t-T) psi(t), psi(0) = phi0
%  
% with psi(t) = phi(T-t)
%
% with the Backward Euler method. The implicit
% difference equation is solved using Newton iteration.
%
% The program assumes you have already computed the solution
% of u. Input ->  (U, t).
% 
% This program uses the function dual.m which specifies
% the rhs of the dual problem  J^t(t), and 
% BackwardEulerNewton for computing the dual solution.
%
% Input: Up  - the computed solution to the IVP
%        tp  - the time values corresponding to the 
%              computed solution
%        f  - the function spec. the ODE
%        T  - the final time
%        Phi0 - the "initial" value of the dual sol.
%
%   After, try plot(Ts, Sc)
%------------------------------------------------------
% Set global variables to be used in the dual solution

global Up fp tp Tp N

% Up - the solution U
% fp - the function specifying the problem, e.g. 'akzonobelf' 
% Tp - the final time T
% tp - all the timevalues
% N - size of the system, u_1, ... u_N
tp = t;
Up = U;
Tp = T;
fp = f;

% Nr of dual solutions to be computed (values of T)
ns=10;
Ts = [linspace(T/(ns^2),T/ns-T/(ns^2),ns-1) linspace(T/ns,T,ns)];
ns = length(Ts);

Sc = zeros(size(Ts));
                                                                               
N  = size(Phi0,1); % Size of the system
m = size(U,2);


for i = 1:ns
   t0 = T - Ts(i)    
   [time, Psi] = BackwardEulerNewton(Phi0, ...
	[t0 T], 'dual');   % Compute the dual solution for a certain
				 % final time T.
  
                                                                         
   DPsi = zeros(1,size(Psi,2)); 
   for j = 2:size(Psi,2)
        DPsi(j) = norm( ( Psi(:,j) - Psi(:,j-1) ) ); % compute the derivative
						     % of the dual solution.
   end
                                               
   Sc(i) = sum(DPsi)      % compute the integral of dpsi = Sc.
end
