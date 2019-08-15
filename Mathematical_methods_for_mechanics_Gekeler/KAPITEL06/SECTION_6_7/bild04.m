function bild04
% Trace of top on surface of unit sphere
clf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% YY(K,:) = [THETA,D_THETA,PHI]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten3 T Y Parmeter THETA1 THETA2
YY = Y'; [M,N] = size(YY); ZZ = zeros(3,N);
for I = 1:N
    THETA = YY(1,I); PHI = YY(3,I);
    ZZ(:,I) = [sin(THETA)*sin(PHI); -sin(THETA)*cos(PHI); cos(THETA)];
end
az = 100; el = 30;  % gut
TT   = linspace(0,2*pi,100);
% -- Grosskreis in Cameraposition ------------
DD = huelle(az,el);
plot3(DD(1,:),DD(2,:),DD(3,:),'k','LineWidth',1.5), hold on
view([az,el])
% -- Grosskreis in (x,z) - Ebene ----------
X = cos(TT);Y = 0*cos(TT);Z = sin(TT);
%plot3(X,Y,Z,'g','LineWidth',1.5), hold on
% -- Grosskreis in (y,z)-Ebene ----------
X = 0*cos(TT);Y = -1*cos(TT);Z = sin(TT);
%plot3(X,Y,Z,'g','LineWidth',1.5), hold on
% -- Spur der Kreiselachse ------------------
plot3(ZZ(1,1),ZZ(2,1),ZZ(3,1),'.','MarkerSize',6), hold on
plot3(ZZ(1,:),ZZ(2,:),ZZ(3,:),'r','linewidth',2), hold on
% -- Breitenkreis am Aequator -------------
THETA = 0;
X = cos(TT)*cos(THETA); Y = sin(TT)*cos(THETA);
Z = sin(THETA)*ones(length(TT),1);
plot3(X,Y,Z,'b','linewidth',2), hold on
if THETA1 ~= 0
   % -- Breitenkreis fuer THETA1 -------------
   THETA = 0.5*pi- THETA1;
   X = cos(TT)*cos(THETA); Y = sin(TT)*cos(THETA);
   Z = sin(THETA)*ones(length(TT),1);
   plot3(X,Y,Z,'k','linewidth',2), hold on
end
if THETA2 ~= 0
   % -- Breitenkreis fuer THETA2 -------------
   THETA = 0.5*pi-THETA2;
   X = cos(TT)*cos(THETA); Y = sin(TT)*cos(THETA);
   Z = sin(THETA)*ones(length(TT),1);
   plot3(X,Y,Z,'k','linewidth',2), hold on
end
plot3(0,0,1,'.','markersize',12), hold on
plot3(0,0,-1,'.','markersize',12), hold on

% -- ACHSEN -----------------------
%X1 = [-2.4, 2.4]; Y1 = [0 0]; Z1 = [0 0]; % X-Achse
%X2 = [0 0]; Y2 = [-1.4 1.4]; Z2 = [0 0];  % Y-Achse
%X3 = [0 0]; Y3 = [0 0]; Z3 = [-1.5 1.5];  % Z-Achse
%plot3(X1,Y1,Z1,'k--','linewidth',2), hold on
%plot3(X2,Y2,Z2,'k--','linewidth',2), hold on
%plot3(X3,Y3,Z3,'k--','linewidth',2), hold on
axis equal tight
%xlabel('e_1-Achse','fontsize',18)
%zlabel('e_3-Achse','fontsize',18)
axis off
