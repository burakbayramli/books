% Scheibe 16, Kreis mit Ecke
clf
L = 10;           % Kantenlaenge des Quadrats
R = 2*L/(3*pi+4); % Radius des Kreises
NN  = 16;         % Anzahl Intervalle auf Kreis
MM = 8;
M  = [-L/2+R;0];     % Mittelpunkt
FAKTOR = 1.2;     % Bildgroesse
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];
TT1    = linspace(pi,7*pi/4,NN);
SEGM1 = R*[cos(TT1);sin(TT1)];
TT2   = linspace(0,1,MM);
A = SEGM1(:,NN); B = [sqrt(2*R*R);0]; C = [SEGM1(1,NN);-SEGM1(2,NN)];
SEGM2 = A*ones(1,MM) + (B - A)*TT2;
SEGM3 = B*ones(1,MM) + (C - B)*TT2;
TT3 = linspace(pi/4,pi,NN);
SEGM4 = R*[cos(TT3);sin(TT3)];
SEGM1 = SEGM1(:,1:NN-1);
SEGM2 = SEGM2(:,1:MM-1);
SEGM3 = SEGM3(:,1:MM-1);
SEGM4 = SEGM4(:,1:NN-1);
X     = [SEGM1,SEGM2,SEGM3,SEGM4];
X     = X + M*ones(1,size(X,2));
X     = X(:,1:size(X,2)-1);
plot(X(1,:),X(2,:)), hold on
axis equal, grid on
%for I = 1:size(X,2)
%   plot(X(1,I),X(2,I),'.k'), hold on
%   pause
%end
save scheibe16 M R X
