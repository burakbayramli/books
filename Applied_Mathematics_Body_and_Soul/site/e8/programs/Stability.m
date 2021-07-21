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

%----------------------------
% THIS IS WHAT YOU MUST DO
%--------

% * Create a for loop for each time T you want to evaluate.
% * Compute the dualsolution for that T.
% * Compute the derivative of the dual function.
% * Compute the stability factor.
