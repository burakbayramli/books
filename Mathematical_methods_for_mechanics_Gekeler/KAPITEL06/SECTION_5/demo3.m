function demo3    
% Computes trajectories of two-body problem
% Dimensionsless differential system
% See Acheson, From Calculus to Chaos.
% INPUT m1, m2, m3 Masses
%       X(1,:) X-coordinates of 2 initial points
%       X(2,:) Y-coordinates of 2 initial points
%       V(1,:) X-coordinates of 2 initial velocities
%       V(2,:) Y-coordinates of 2 initial velocities
% OUTPUT: trajectories n CARTESISIAN coordinates
clc, format compact
example = 100;
while ~ismember(example,[1,2,3])
   example   = input(' Example No. (1/2/3) ');
end;
switch example
case 1, %same mass, circular oribts --------------
   m1 = 0.5; m2 = 0.5;  TEND = 3;
   rr = 0.03; % nur fuer Grafik
   % -- Data for corners of figure
   l_u_r_o = [-1.2, -1.1, 1.2, 1.1];
   % ---------------------------
   Parmeter = [m1, m2, rr, l_u_r_o];
   X = [-m2, m1; 0, 0];    % initial position
   V = [0, 0; -0.5, 0.5];  % initial velocity
case 2, % different masses two cocentric orbits ---
   m1 = 7/10; m2 = 3/10;  TEND = 3;
   rr = 0.03; % nur fuer Grafik
   % -- Data for corners of figure
   l_u_r_o = [-1.2, -1.1, 1.2, 1.1];
   % ---------------------------
   Parmeter = [m1, m2, rr, l_u_r_o];
   X = [-m2, m1; 0, 0];    % initial position
   V = [0, 0; -m2, m1];  % initial velocity.
case 3, %as case 2 but no track velocities ---
   m1 = 7/10; m2 = 3/10;  TEND = 10;
   rr = 0.03; % nur fuer Grafik
   % -- Data for corners of figure
   l_u_r_o = [-1.2, -1.1, 1.2, 1.1];
   % ---------------------------
   Parmeter = [m1, m2, rr, l_u_r_o];
   X = [-m2, m1; 0, 0];    % initial position
   V = [0, 0; -m2, m1];  % initial velocity.
   % Test alternatives: ---------
   %V = V/2;
   %X(1) = X(1) - 0.5; V = V -0.2;
end
TT      = linspace(0,TEND,300);
XSTART  = [X(1,1:2)';X(2,1:2)';V(1,1:2)';V(2,1:2)'];   
options = odeset('Reltol',1E-8);
[T,Y]   = ode45(@bsp03,TT,XSTART,options,Parmeter);
Y       = Y(:,1:6);
save daten3 T X V Y Parmeter example
bild03a
