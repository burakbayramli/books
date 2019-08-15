% Scheibe 20, Komplement von Hundeknochen
% Abstand zwischen Mittelpunkten nicht konstant
clf
clc
% -- Hundeknochen ---------------------------------
% -- Parameter --------------------------
LL = 2; % Abstand der Zentren
RR = sqrt(2*LL*LL)/2;
NN    = 11;  % Anzahl Intervalle grossem Segment
MM = 7;
% -----------------------------------
M     = [0;0]; % Mittelpunkt
R    = 2*L;    % Radius
FAKTOR = 1.2;  % Bildgroesse
TT = linspace(pi,7*pi/4,NN); SEGM1 = [RR*cos(TT);RR*sin(TT)];
M1 = [-LL;0]; SEGM1 = M1*ones(1,NN) + SEGM1;
SEGM1 = SEGM1(:,1:NN-1);
TT = linspace(3*pi/4,pi/4,MM); SEGM2 = [RR*cos(TT);RR*sin(TT)];
M2 = [0;-LL]; SEGM2 = M2*ones(1,MM) + SEGM2;
SEGM2 = SEGM2(:,1:MM-1);
TT = linspace(-3*pi/4,3*pi/4,2*NN); SEGM3 = [RR*cos(TT);RR*sin(TT)];
M3 = [LL;0]; SEGM3 = M3*ones(1,2*NN) + SEGM3;
SEGM3 = SEGM3(:,1:2*NN-1);
TT = linspace(-pi/4,-3*pi/4,MM); SEGM4 = [RR*cos(TT);RR*sin(TT)];
M4 = [0;LL]; SEGM4 = M4*ones(1,MM) + SEGM4;
SEGM4 = SEGM4(:,1:MM-1);
TT = linspace(pi/4,pi,NN); SEGM5 = [RR*cos(TT);RR*sin(TT)];
M5 = [-LL;0]; SEGM5 = M1*ones(1,NN) + SEGM5;
SEGM5 = SEGM5(:,1:NN-1);
X = [SEGM1,SEGM2,SEGM3,SEGM4,SEGM5];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
LAENGE = sqrt(X(1,:).^2 + X(2,:).^2);
LX = size(X,2);
ABSTAND = 2*LL;
ABDRUCK = ABSTAND - LAENGE;
TT = linspace(0,2*pi,LX);
Y = [ABDRUCK.*cos(TT);ABDRUCK.*sin(TT)];
U = [LAENGE.*cos(TT);LAENGE.*sin(TT)];


Z = [ABSTAND.*cos(TT);ABSTAND.*sin(TT)];
% -- Grafik -----------------------------------

axis([-8 8 -8 8])
axis equal, axis manual, grid on, hold on
plot(X(1,:),X(2,:),'b'), hold on
plot(X(1,:),X(2,:),'.b'), hold on

plot(Y(1,:),Y(2,:),'r'), hold on
plot(Y(1,:),Y(2,:),'.r'), hold on
plot(U(1,:),U(2,:),'k'), hold on
plot(U(1,:),U(2,:),'.k'), hold on

plot(Z(1,:),Z(2,:),'k'), hold on
plot(Z(1,:),Z(2,:),'.k'), hold on

flag = 0;
if flag == 1
   for I = 1:size(Y,2)
      plot(Y(1,I),Y(2,I),'.r'), hold on
      pause
   end
end
M = MA; R = 2*LL;
save scheibe21 M R X
