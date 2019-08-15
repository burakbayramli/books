% Scheibe 7, Grosses Quadrat
clf
NN  = 11;      % Anzahl Intervalle auf Segment
M  = [0;0];    % Mittelpunkt
R  = sqrt(50); % Radius
KK = 5;        % halbe Kantenlaenge
FAKTOR = 1.2;  % Bildgroesse
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];

SEGM1 = [linspace(KK,-KK,NN); KK*ones(1,NN)];
SEGM1 = SEGM1(:,1:NN-1);
phi = pi/2; DD  = [cos(phi), -sin(phi); sin(phi), cos(phi)];
SEGM2 = DD*SEGM1; SEGM3 = DD*SEGM2; SEGM4 = DD*SEGM3;
X = [SEGM2,SEGM3,SEGM4,SEGM1];
X = [X(:,6:size(X,2)),X(:,1:6)];
X = X(:,1:size(X,2)-1);
X = disc_aendern(X,LR,UO);
save scheibe07 M R X
