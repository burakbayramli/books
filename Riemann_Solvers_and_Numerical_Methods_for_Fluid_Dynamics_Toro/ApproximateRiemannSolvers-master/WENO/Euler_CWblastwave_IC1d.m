function [r0,u0,p0,tEnd,cfl] = Euler_CWblastwave_IC1d(x)
% Load the IC of a classical 1D Riemann Problems.
%
% Coded by Manuel A. Diaz, NTU, 2013.1.24.
%
% The proporties of the problem are arranged as:
%
%       prop = [prop_left ,prop_middle, prop_right]
% 
% Notation:
% r = Density,
% u = Velocity in x-direction,
% p = Pressure.
%
%% Colella Woodward blastwave IC
fprintf('Shocktube problem with supersonic zone');
r = [1    1   1  ];
u = [0    0   0  ];
p = [1000 0.1 100];
tEnd = 0.038; cfl = 0.90; 

% Print for Riemann Problems
fprintf('\n');
fprintf('density (L): %1.3f\n',r(1));
fprintf('velocity(L): %1.3f\n',u(1));
fprintf('Presure (L): %1.3f\n',p(1));
fprintf('\n');
fprintf('density (M): %1.3f\n',r(2));
fprintf('velocity(M): %1.3f\n',u(2));
fprintf('Presure (M): %1.3f\n',p(2));
fprintf('\n');
fprintf('density (R): %1.3f\n',r(3));
fprintf('velocity(R): %1.3f\n',u(3));
fprintf('Presure (R): %1.3f\n',p(3));
fprintf('\n');

%% Load Selected case Initial condition:
% Pre-Allocate variables
r0 = zeros(size(x)); 
u0 = zeros(size(x)); 
p0 = zeros(size(x));

% Parameters of regions dimensions
L = find(x<0.1);
M = find(x>=0.1 & x<=0.9);
R = find(x>0.9);

% Initial Condition for our 1-d domain
% Density
r0(L) = r(1); % region 1
r0(M) = r(2); % region 2
r0(R) = r(3); % region 3
% Velovity in x
u0(L) = u(1); % region 1
u0(M) = u(2); % region 2
u0(R) = u(3); % region 3
% pressure
p0(L) = p(1); % region 1
p0(M) = p(2); % region 2
p0(R) = p(3); % region 3