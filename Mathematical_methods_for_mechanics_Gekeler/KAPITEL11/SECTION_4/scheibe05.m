% Scheibe 5, Malteser-Kreuz
clf
% -- Parameter --------------------------
r    = 1;    % Radius des Kreissegments: VORGEBEN!
NN    = 11;  % Anzahl Intervalle auf Kreissegment
% -----------------------------------
M     = [0;0]; % Mittelpunkt
 R    = sqrt(3);     % Radius
FAKTOR = 1.2;  % Bildgroesse
alfa  = pi/6; L = 2; r = 1; gama = pi/3;
TT    = linspace(gama,-gama,NN);
SEGM1 = [-L + r*cos(TT);r*sin(TT)];
SEGM1 = SEGM1(:,1:NN-1);
phi   = 2*alfa;
DD    = [cos(phi), -sin(phi); sin(phi), cos(phi)];
SEGM2 = DD*SEGM1; SEGM3 = DD*SEGM2; SEGM4 = DD*SEGM3;
SEGM5 = DD*SEGM4; SEGM6 = DD*SEGM5;
X = [SEGM1,SEGM2,SEGM3,SEGM4,SEGM5,SEGM6];
X = [X(:,6:size(X,2)),X(:,1:6)];
axis equal, grid on, hold on
plot(X(1,:),X(2,:),'k',X(1,:),X(2,:),'.k')
circle(X(1,1),X(2,1),0.05,'b')
save scheibe05 M R X
