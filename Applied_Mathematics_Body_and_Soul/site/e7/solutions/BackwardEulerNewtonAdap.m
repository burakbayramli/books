function [t, U] = BackwardEulerNewtonAdap(U0, I, f, TOL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Solves the first-order ODE system u' = f(t,u), u(0) = U0
% using the backward Euler method. The implicit difference
% equation is solved using Newton iteration.'
%
% Input:          U0 - startvector
%                 I  - interval [t0, tN]
%                 f  - a string with the function name, e.g. 'testfunction'
%                 TOL - error tolerance 
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
  i = 1;
  Sc = 1;


  while(t(i) < T)
    if i > 1
        k = TOL/(Sc*norm(U(:,i)-U(:,i-1))/k);
    else
        k=0.01;
    end
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
%          equation is solved using Newton-Raphson iteration.
%
%====
function U1 = StepBE(U0, tn, kn, f)


 % Takes one backward Euler step
 

  TOL = 0.001;
 
  U1=U0;
  Uold = U0 + ones(length(U0),1);
 

  while abs(U1 - Uold) > TOL
     Uold = U1;
     delta = gfcnjacobian(tn, U1, kn, f)\gfcn(tn, U0, U1, kn, f);

     U1 = U1 - delta;

  end

% End of function StepBE

%=============================================================
%
%  Matlab function gfcn
%
%  Purpose: This routine computes the function
%           g(u_{n+1}) = u_{n+1} - u_n - hn*f( tn+kn, u_n )
%
%           which is used for newton-Raphson iteration
%======
function gfcnvalue = gfcn(tn, U0, U1, kn, f)

     gfcnvalue = U1 - U0 - kn*feval(f, tn+kn, U1)';

% End of function gfcn
%=============================================================
%
%  Matlab function gfcnjacobian
%
%  Purpose: This routine computes the jacobian (w.r.t. U1) of
%           the function gfcn
%======
function gfcnJ = gfcnjacobian(tn, U1, kn, f)

n = length(U1);
J = zeros(n);
Uh = zeros(n,1);
I = eye(n);

for i = 1:n
    Uh = U1;
    Uh(i) = U1(i)+kn;
    Jup = (feval(f, tn+kn, Uh)' - feval(f, tn+kn, U1)')/kn;
    J(:,i) = Jup;
end

gfcnJ = I - kn*J;

% End function gfcnjacobian
%=============================================================
