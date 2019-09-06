% Program for solving Keplers equation by Newton--Raphson iteration.
function E=kepler(e,M);
eps=1e-10; % Tolerance
% Initial guess:
B=cos(e)-(pi/2-e)*sin(e);
E=M+e*sin(M)/(B+M*sin(e));
fE=E-e*sin(E)-M;
% Iteration for E follows:
while abs(fE)>eps
fE=E-e*sin(E)-M;
fpE=1-e*cos(E);
dE=-fE/fpE;
E=E+dE;
end
