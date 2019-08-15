% Scheibe 15, Malteser-Kreuz, Komplement
clf
% -- Parameter --------------------------
sq3 = sqrt(3);
M  = [-2;0];   % Mittelpunkt
R = 2/sq3; FAKTOR = 1.2;
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];
MM = 21;
alfa  = pi/6; L = 2; r = 1; gama = pi/3;
TT    = linspace(-gama,gama,MM);
SEGM1 = [-L + r*cos(TT);r*sin(TT)];
LAENGE = sqrt(SEGM1(1,:).^2 + SEGM1(2,:).^2);
RADIUS = 2 - LAENGE; NN = length(RADIUS);
TT = -linspace(-pi/3,pi/3,NN);
SEGM1 = [RADIUS.*cos(TT);RADIUS.*sin(TT)];
SEGM1 = SEGM1(:,1:size(SEGM1,2)-1);
cs = cos(2*pi/3); ss = sin(2*pi/3); DD = [cs,ss; -ss, cs];
SEGM2 = DD*SEGM1; SEGM3 = DD*SEGM2;
Y = [SEGM1,SEGM2,SEGM3];
Y = [Y(:,11:size(Y,2)),Y(:,1:10)];
X = Y + M*ones(1,size(Y,2));
plot(X(1,:),X(2,:)), hold on
axis equal, grid on
%for I = 1:size(X,2)
%    plot(X(1,I),X(2,I),'.k'), hold on
%    pause
%end

save scheibe15 M R X
