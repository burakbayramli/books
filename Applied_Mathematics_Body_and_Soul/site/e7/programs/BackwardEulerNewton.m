function [t, U] = BackwardEulerNewton(U0, I, f)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Solves the first-order ODE system u' = f(t,u), u(0) = U0
% using the backward Euler method. The implicit difference
% equation is solved using Newton iteration.'
%
% Input:          U0 - startvector
%                 I  - interval [t0, tN]
%                 f  - a string with the function name, e.g. 'testfunction'
%
% Output:         t  - the time points
%                 U  - the solution
%                 
% To see the result, try 
% >> plot(t,U)  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  t = I(1);
  T = I(2);
  U = U0;
  % Fixed stepsize
  k = 0.01;
  i = 1;

  while(t(i) < T)
    t(i + 1) = t(i) + k;
    U(:, i + 1) = StepBE(U(:, i), t(i), k, f);
    i = i + 1;
  end




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%                     SUBROUTINES                            %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%=============================================================
%
% Matlab function StepBE. 
%
% Purpose: This routine computes one step with the Backward 
%          Euler method. The implicit difference
%          equation is solved using Newton iteration.
%
%====
function U1 = StepBE(U0, tn, kn, f)


 % Takes one backward Euler step
 

  TOL = 0.001;
 
  U1=U0;
  Uold = U0 + ones(length(U0),1);
 

  while abs(U1 - Uold) > TOL
     Uold = U1;
     delta = ...   % <------ You must edit here in order for the solver
                   % to work properly. This is the equation for a
                   % a Newton step.

     U1 = U1 - delta;

  end

% End of function StepBE

