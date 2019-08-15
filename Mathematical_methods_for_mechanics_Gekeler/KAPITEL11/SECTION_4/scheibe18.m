% Scheibe 18, kleines Zahnrad
% Abdruck vom grossen Zahnrad
clf
clc
% -- GROSSES ZAHNRAD -----------------------
R   = 3; UB = 2*pi*R;    %Radius/Umfang
NN  = 24; PHI = 2*pi/24; % Anzahl Zaehne
M   = [0;0]; sq3 = sqrt(3);
BASIS = 2*R*sin(PHI/2); HOEHE = BASIS/2;
SKAL_H = 1.01;
HOEHE = SKAL_H*HOEHE;
X1    = [BASIS/2;0]; X2 = [0;HOEHE]; X3 = -X1;
SEGM1 = [X1,X2,X3] + [0;R]*ones(1,3); % Gleichseitiges Dreieck
psi   = PHI/2; cs = cos(psi); ss = sin(psi);
DD    = [cs, -ss; ss, cs];
SEGM1 = DD*SEGM1; SEGM1 = SEGM1(:,1:size(SEGM1,2)-1);
    X = SEGM1;
for I = 2:NN
    SEGM2 = DD*DD*SEGM1;
    X = [X,SEGM2];
    SEGM1 = SEGM2;
end
cs = cos(pi/2); ss = sin(pi/2); DD = [cs, -ss; ss, cs];
X  = DD*X;
% -- Kleines Zahnrad
MM = size(X,2);
MM = MM/2- 1;
LAENGE = sqrt(X(1,1:MM).^2 + X(2,1:MM).^2);
SKAL = (R+HOEHE)/R;
SKAL = 1.11
RB = R; RA = SKAL*R/2 % RADIUS vom kleine ZAHNRAD
ABDRUCK = RA + RB - LAENGE;
TT = -linspace(0,2*pi,MM);
Y = [ABDRUCK.*cos(TT);ABDRUCK.*sin(TT)];
X = Y;
M = [-(RA + RB);0];
R = RA;
X = X + M*ones(1,size(X,2));
FAKTOR = 1.2;     % Bildgroesse
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
axis([-8 8 -8 8])
grid on, axis equal, axis manual, hold on

plot(X(1,:),X(2,:)), hold on
%for I = 1:size(X,2)
%   plot(X(1,I),X(2,I),'.k'), hold on
%   pause
%end
save scheibe18 M R X
