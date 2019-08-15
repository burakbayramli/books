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
R    = 2*LL;    % Radius
FAKTOR = 1.2;  % Bildgroesse
TT = linspace(pi,7*pi/4,NN); SEGM1 = [RR*cos(TT);RR*sin(TT)];
M1 = [-LL;0]; SEGM1 = M1*ones(1,NN) + SEGM1;
SEGM1 = SEGM1(:,1:NN-1);

TT = linspace(3*pi/4,pi/4,MM); SEGM2 = [RR*cos(TT);RR*sin(TT)];
M2 = [0;-LL]; SEGM2 = M2*ones(1,MM) + SEGM2;
SEGM2 = SEGM2(:,1:MM-1);

TT = linspace(-3*pi/4,0,NN); SEGM3 = [RR*cos(TT);RR*sin(TT)];
M3 = [LL;0]; SEGM3 = M3*ones(1,NN) + SEGM3;
SEGM3 = SEGM3(:,1:NN-1);

TT = linspace(0,3*pi/4,NN); SEGM4 = [RR*cos(TT);RR*sin(TT)];
M4 = [LL;0]; SEGM4 = M4*ones(1,NN) + SEGM4;
SEGM4 = SEGM4(:,1:NN-1);

TT = linspace(-pi/4,-3*pi/4,MM); SEGM5 = [RR*cos(TT);RR*sin(TT)];
M5 = [0;LL]; SEGM5 = M5*ones(1,MM) + SEGM5;
SEGM5 = SEGM5(:,1:MM-1);

TT = linspace(pi/4,pi,NN); SEGM6 = [RR*cos(TT);RR*sin(TT)];
M6 = [-LL;0]; SEGM6 = M6*ones(1,NN) + SEGM6;
SEGM6 = SEGM6(:,1:NN-1);


X = [SEGM1,SEGM2,SEGM3,SEGM4,SEGM5,SEGM6];




DD = [0, -1; 1, 0];
Y = DD*X;
MA = [-2*LL;0];
Y = Y + MA*ones(1,size(Y,2));
Y = fliplr(Y);
LY = size(Y,2);
START = 39;
Y = [Y(:,START:LY),Y(:,1:START-1)];
% -- Grafik -----------------------------------

axis([-8 8 -8 8])
axis manual, grid on, hold on
plot(X(1,:),X(2,:),'b'), hold on
%plot(X(1,:),X(2,:),'.b'), hold on

plot(Y(1,:),Y(2,:),'r'), hold on
%plot(Y(1,:),Y(2,:),'.r'), hold on

flag = 0;
if flag == 1
   for I = 1:size(Y,2)
      plot(Y(1,I),Y(2,I),'.r'), hold on
      pause
   end
end
M = MA; R = 2*LL; X = Y;
save scheibe20 M R X
