% Scheibe 14, Komplement zu Malteserkreuz
% bei Drehung
clf
NN = 31;
sq3 = sqrt(3);
M  = [-1-2/sq3;0];   % Mittelpunkt
R = 2/sq3; FAKTOR = 1.2;
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];

X1 = [2/sq3;0]; X2 = [-1/sq3; -1]; X3 = [-1/sq3;1];
TT = -linspace(pi/6,pi/2,NN);
K1 = ((X1+X3)/2)*ones(1,NN) + [cos(TT);sin(TT)];
K2 = ((X2+X3)/2)*ones(1,NN) + [cos(TT);sin(TT)];
SEGM1 = [K1(:,1:NN-1),K2(:,1:NN-1)];
cs = cos(2*pi/3); ss = sin(2*pi/3); DD = [cs,ss; -ss, cs];
SEGM2 = DD*SEGM1; SEGM3 = DD*SEGM2;
Y = [SEGM1,SEGM2,SEGM3];
X = Y + M*ones(1,size(Y,2));
plot(X(1,:),X(2,:)), hold on
axis equal, grid on
save scheibe14 M R X
