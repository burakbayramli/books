% Scheibe 19, Hundeknochen
clf
clc
% -- Parameter --------------------------
LL = 2; % Abstand der Zentren
RR = sqrt(2*LL*LL)/2;
NN    = 11;  % Anzahl Intervalle grossem Segment
MM = 7;
% -----------------------------------
M     = [0;0]; % Mittelpunkt
R    = 2*LL;    % Radius
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
axis([-8 8 -8 8])
axis manual, grid on, hold on
plot(X(1,:),X(2,:),'k'), hold on
%for I = 1:size(X,2)
%   plot(X(1,I),X(2,I),'.k'), hold on
%   pause
%end
M = [0;0]; R = 2*LL + 2*RR;
save scheibe19 M R X
