function demo2
% Computes trajectories of three-body problem
% Dimensionsless differential system
% See Acheson, From Calculus to Chaos.
% INPUT m1, m2, m3 Masses
%       X(1,:) X-coordinates of 3 initial points
%       X(2,:) Y-coordinates of 3 initial points
%       V(1,:) X-coordinates of 3 initial velocities
%       V(2,:) Y-coordinates of 3 initial velocities
% OUTPUT: trajectories n CARTESISIAN coordinates
clc, format compact
example = 100;
while ~ismember(example,[1,2,3,4,5,6])
   example   = input(' Example No. (1/2/3/4/5/6) ');
end;
switch example
case 1, % -- Parameter --------------
   m1 = 81/82; m2 = 1/82; m3 = 0; TEND = 13;
   rr = 0.03; % nur fuer Grafik
   % -- Data for corners of figure
   l_u_r_o = [-1.2, -1.1, 1.2, 1.1];
   % ---------------------------
   Parmeter = [m1, m2, m3, rr, l_u_r_o];
   X = [-m2, m1, 1.05; 0, 0, 0];    % initial position
   V = [0, 0, 0; -m2, m1, 0.2012];  % initial velocity.
case 2, % -- Parameter --------------
   m1 = 0.5; m2 = 0.5; m3 = 0; TEND = 25; 
   rr = 0.035; l_u_r_o = [-1.8, -1.4, 1.8, 1.4];
   % ---------------------------
   Parmeter = [m1, m2, m3, rr, l_u_r_o];
   X = [-m2, m1, -1; 0, 0, 0];
   V = [0, 0, 0; -m2, m1, 0.58];
case 3, % -- Parameter --------------
   m1 = 0.5; m2 = 0.5; m3 = 0; TEND = 25;
   rr = 0.035; l_u_r_o = [-1.8, -1.4, 1.8, 1.4];
   % ---------------------------
   Parmeter = [m1, m2, m3, rr, l_u_r_o];
   X = [-m2, m1, -1; 0, 0, 0];
   V = [0, 0, 0; -m2, m1, 0.506];
case 4, % -- Parameter --------------
   m1 = 0.5; m2 = 0.5; m3 = 0.01; TEND = 25;
   rr = 0.035; l_u_r_o = [-1.8, -1.4, 1.8, 1.4];
   % ---------------------------
   Parmeter = [m1, m2, m3, rr, l_u_r_o];
   X = [-m2, m1, -1; 0, 0, 0];
   V = [0, 0, 0; -m2, m1, 0.59];
case 5, % Acheson, p. 95  ------------
   m1 = 0.5; m2 = 0.5; m3 = 0.5; TEND = 8;
   rr = 0.035; l_u_r_o = [-1.8, -1.4, 1.8, 1.4];
   % ---------------------------
   Parmeter = [m1, m2, m3, rr, l_u_r_o];
   X = [-0.5, 0.5, -0.1; 0, 0, 0.75];
   V = [0, 0, 0; -0.5, 0.5, -0.3];
case 6, % Acheson, p. 95  ------------
   m1 = 0.5; m2 = 0.5; m3 = 0; TEND = 25;
   rr = 0.035; l_u_r_o = [-1.8, -1.4, 1.8, 1.4];
   % ---------------------------
   Parmeter = [m1, m2, m3, rr, l_u_r_o];
   X = [-0.5, 0.5, -1; 0, 0, 0];
   V = [0, 0, 0; -0.5, 0.5, 0.41];

end
TT      = linspace(0,TEND,300);
XSTART  = [X(1,1:3)';X(2,1:3)';V(1,1:3)';V(2,1:3)'];  
options = odeset('Reltol',1E-8);
[T,Y]   = ode45(@bsp02,TT,XSTART,options,Parmeter);
Y       = Y(:,1:6);
save daten2 T X V Y Parmeter example
bild02a
pause
% alternative in rotating system 
if ismember(example,[1,2,3,4,6])
   bild02b
else
  disp('Rotating system does not make sense')
end  
