% Scheibe 13, Gleichseitiges Dreieck
clf
sq3 = sqrt(3);
M  = [-1-2/sq3;0];   % Mittelpunkt
R = 2/sq3; FAKTOR = 1.2;
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];

X1 = [2/sq3;0]; X2 = [-1/sq3; -1]; X3 = [-1/sq3;1];
X = [X1,(X1+X2)/2,X2,(X2+X3)/2,X3,(X3+X1)/2];
%X = [X1,X2,X3];

X = X + M*ones(1,3);
X = disc_aendern(X,LR,UO);
save scheibe13 M R X
