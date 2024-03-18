function [Msub,Msup] = am(A,gamma,Astar)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculates the supersonic and the subsonic 
% solution to the area-mach relation
% for a given A and gamma. If Astar is
% not passed to the function, A is considered
% as the ratio A/Astar.
%
% Input: A = area
%        gamma = cp/cv = 1.4 for air
%        Astar = throat area
%
% Output: Msub = subsonic solution
%         Msup = supersonic solution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if nargin == 2
  Astar = 1;
end

% Set initial values for bisection method
tol   = 1E-5;
M0sup = 1;
M0sub = 1;
Mstep = 0.2;
ind   = 0;

% Start with supersonic solution
while Mstep > tol & ind < 100
  ind = ind + 1;
  Msup = M0sup + Mstep;
%  fval = feval('fkn_exact',Msup,A,gamma,Astar);
  fval = fkn_exact(Msup,A,gamma,Astar);
  if fval < 0
    M0sup = Msup;
  else
    Mstep = Mstep/2;
  end
end
iter_sup = ind;

% Msup contains the Mach number for the supersonic solution,
% the same as Table A.1 in Appendix A.

% Find the subsonic solution
Mstep = 0.2;
ind = 0;
while Mstep > tol & ind < 100
  ind = ind + 1;
  Msub = M0sub - Mstep;
  %fval = feval('fkn_exact',Msub,A,gamma,Astar);
    fval = fkn_exact(Msub,A,gamma,Astar);

  if fval < 0
    M0sub = Msub;
  else
    Mstep = Mstep/2;
  end
end
iter_sub = ind;

% Msub contains the Mach number for the subsonic solution,
% the same as Table A.1 in Appendix A.
