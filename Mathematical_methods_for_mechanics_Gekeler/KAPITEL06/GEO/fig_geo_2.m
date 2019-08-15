function fig_geo_2
% Shortest path on earth connection between two towns 
% Examples:
% Lissabon      (38^o.44'N|9^o.08' W)
% Rio de Janeiro (22^o.53'S|43^o.17'W)
% Pei-Ching      (39^o.55'N|116.25 O)
% Stuttgart      (48^o.47'N|9^o.12'O)
% North positive , South negative , East positive, West negative
% degrees of longitude are clockwise counted, seen from north pole 
% Greenwich has longitude zero 
% -- Stuttgart --------------------------
L1 = (9 + 12/60)*pi/180;     % longitude
L2 = (48 + 47/60)*pi/180;    % latitude
% -- Lissabon --------------------------------
%L1 =  - (9 + 8/60)*pi/180;  % longitude
%L2 = (38 + 44/60)*pi/180;   % latitude
% -- Pei-Ching (Peking) --------------------------
J1 = (116 + 25/60)*pi/180;  % longitude
J2 = (39 + 55/60)*pi/180;   % latitude 
% -- Rio de Janeiro -----------------------------------------
%J1 =  - (43+ 10/60)*pi/180;  % longitude 
%J2 = -(22 + 53/60)*pi/180;   % latitude
% --------------------------------------------------

clc, clf 
az = 100;
%az = -37.5; 
%az = 90;
el = 30; % well
%el = 0;
k = 5; n = 2^k-1; l = 4; m = 2^l-1;
phi   = pi*(0:2:2*n)/n;
theta =(pi/2)*(-m:2:m)'/m;
psi   = pi*(-m:2:m)/m;

% --------------------------------
h = axes('Position',[0 0 1 1]);
%axes('Position',[0 0 0.8 1])
% AXES -------------------------
X = [-2.4, 2.4]; Y = [0 0]; Z = [0 0];
view([az,el])
plot3(X,Y,Z,'k--'), hold on
X = [0 0]; Y = [-1.4 1.4]; Z = [0 0];
plot3(X,Y,Z,'k--'), hold on
X = [0 0]; Y = [0 0]; Z = [-1.5 1.5];
plot3(X,Y,Z,'k--'), hold on

% Hull of Earth -----------------------
DD = huelle(az,el);
plot3(DD(1,:),DD(2,:),DD(3,:),'k','LineWidth',1.5), hold on

% Meridians in (x,z) and (y,z) plane ----------
X = 1*cos(theta); Y = 0*cos(theta); Z = sin(theta);
plot3(X,Y,Z,'LineWidth',1.5), hold on
X = -1*cos(theta); Y = 0*cos(theta); Z = sin(theta);
plot3(X,Y,Z,'LineWidth',1.5), hold on

X = 0*cos(theta); Y = -1*cos(theta); Z = sin(theta);
plot3(X,Y,Z,'LineWidth',1.5), hold on
X = 0*cos(theta); Y = 1*cos(theta); Z = sin(theta);
plot3(X,Y,Z,'LineWidth',1.5), hold on

flag = 0;
if flag == 1
   % parallels of latitude --------
   M = length(theta);
   for I = 1:length(theta)
      X = cos(phi)*cos(theta(I)); Y = sin(phi)*cos(theta(I));
      Z = sin(theta(I))*ones(length(phi),1);
      plot3(X,Y,Z,'r'), hold on
   end
   % Meridians ---------------------
   N = length(psi);
   for I = 1:N
      X = cos(psi(I))*cos(theta); Y = sin(psi(I))*cos(theta);
      Z = sin(theta);
      plot3(X,Y,Z,'b'), hold on
   end
end
% --------------------------------------------------
PHI   = [L1; J1]; % initial and terminal longitude 
THETA = [L2; J2]; % initial and terminal latitude

A = [cos(PHI(1))*cos(THETA(1)); sin(PHI(1))*cos(THETA(1));
     sin(THETA(1))];

B = [cos(PHI(2))*cos(THETA(2)); sin(PHI(2))*cos(THETA(2));
     sin(THETA(2))];

% Town A 
view(az,el)
plot3(A(1),A(2),A(3),'*','MarkerSize',12), hold on

% parallel of latitude of town A
X = cos(phi)*cos(THETA(1)); Y = sin(phi)*cos(THETA(1));
Z = sin(THETA(1))*ones(length(phi),1);
plot3(X,Y,Z,'r'), hold on

% meridian of town A
X = cos(PHI(1))*cos(theta); Y = sin(PHI(1))*cos(theta);
Z = sin(theta);
plot3(X,Y,Z,'b'), hold on


% Town B
plot3(B(1),B(2),B(3),'o','MarkerSize',6), hold on

% parallel of town B
X = cos(phi)*cos(THETA(2)); Y = sin(phi)*cos(THETA(2));
Z = sin(THETA(2))*ones(length(phi),1);
plot3(X,Y,Z,'r'), hold on

% Meridian of town B
X = cos(PHI(2))*cos(theta); Y = sin(PHI(2))*cos(theta);
Z = sin(theta);
plot3(X,Y,Z,'b'), hold on

% Connection of towns ---------------------
DD = gkreis(PHI,THETA);
plot3(DD(1,:),DD(2,:),DD(3,:),'k','linewidth',3);
hold on
% ------------------------------------------
grid on
%out = get(gca,'Cameraupvector')
axis equal tight
xlabel('x-Achse')
ylabel('y-Achse')
% --------------------------------
%axis off
