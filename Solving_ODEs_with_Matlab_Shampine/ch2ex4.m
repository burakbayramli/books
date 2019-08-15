function ch2ex4
% Define the physical constants:
global kappa beta a Cp K ga sinth Phi Ph Af G0
kappa = 0.171446272015689e-8;
beta  = 0.213024626664637e-2;
a     = 0.108595374561510e+4;
Cp    = 0.496941623289027e+4;
K     = 10;
ga    = 9.80665;
sinth = 1;
Phi   = 1.1e+5;
Ph    = 797.318;
Af    = 3.82760;
G0    = 270.9;
options = odeset('Mass',@mass,'MassSingular','no','Events',@events);
[z,y,ze,ye,ie] = ode45(@odes,[0 5],[795.5; 255.0],options);
if ~isempty(ie)
     fprintf('Upper boundary at z = %g.\n',ze(end));
end
plot(z,y);
%===================================================================
function dydz = odes(z,y)
global kappa beta a Cp K ga sinth Phi Ph Af G0
rho = y(1);
T = y(2);
dydz = [ (-K*G0*abs(G0/rho) - rho*ga*sinth)
         (a^2 *Phi*Ph*kappa)/(Cp*Af)       ];
 
function A = mass(z,y)
global kappa beta a Cp K ga sinth Phi Ph Af G0
rho = y(1);
T = y(2);
A = zeros(2);
A(1,1) = 1/(rho*kappa) - (G0/rho)^2;
A(1,2) = beta/kappa;
A(2,1) = -(a^2 *beta*(T + 273.15)*G0)/(Cp*rho^2);
A(2,2) = G0/rho;

function [value,isterminal,direction] = events(z,y)
isterminal = 1;
direction  = 0;
rho   = y(1);
T     = y(2);
rhosat  = -3.3*(T - 290.0) + 738.0;
value = rho - rhosat;