% Scheibe 22, Schale
clf
clc
NN  = 33;      % Anzahl Intervalle
M  = [0;0];   % Mittelpunkt
R  = 5;       % Radius
FAKTOR = 1.2; % Bildgroesse
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];

TT = linspace(pi,2*pi,NN);
SEGM1 = R*[cos(TT);sin(TT)];
SEGM1 = SEGM1(:,1:NN-1);
TT = linspace(0,pi,NN);
SEGM2 = R*[cos(TT);-sin(TT)];
SEGM2 = SEGM2(:,1:NN-1);
X = [SEGM1,SEGM2];
axis([-8 8 -8 8])
axis equal, axis manual, grid on, hold on
plot(X(1,:),X(2,:),'b'), hold on

flag = 0;
if flag == 1
   for I = 1:size(SEGM1,2)
      plot(X(1,I),X(2,I),'.r'), hold on
      pause
   end
   for I = size(SEGM1,2)+1:size(X,2)
      plot(X(1,I),X(2,I),'or'), hold on
      pause
   end

end

save scheibe22 M R X
