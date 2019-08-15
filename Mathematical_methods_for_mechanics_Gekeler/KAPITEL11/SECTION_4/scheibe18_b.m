% Scheibe 18, kleines Zahnrad
% fuer Innenverzahnung
% Abstand mit SKALE Einstellen!
MB = [0;0]; RB = 3; %Zentrum/Radius Gr. Zahnrad
clf, clc
R      = RB/2; UB = 2*pi*R;               %Radius/Umfang
M      = [0;0]; FAKTOR = 1.4;          % Mittelpunkt/Bildgroesse
NN     = 12; PHI = 2*pi/NN;            % Anzahl Zaehne
MM     = 5; TT     = linspace(0,1,MM); % Zahnflanke
BASIS  = 2*R*sin(PHI/2); HOEHE = BASIS/2;
X1     = [BASIS/2;0]; X2 = [0;HOEHE]; X3 = -X1;
SEGM1A = X1*ones(1,MM) + (X2 - X1)*TT;
SEGM1B = X2*ones(1,MM) + (X3 - X2)*TT;
SEGM1A = SEGM1A(:,1:MM-1); SEGM1B = SEGM1B(:,1:MM-1);
SEGM1  = [SEGM1A,SEGM1B];
SEGM1  = SEGM1 + [0;R]*ones(1,size(SEGM1,2));
psi    = PHI/2; cs = cos(psi); ss = sin(psi);
DD     = [cs, -ss; ss, cs]; SEGM1 = DD*SEGM1;
X      = SEGM1;
for I = 2:NN
    SEGM2 = DD*DD*SEGM1; X = [X,SEGM2]; SEGM1 = SEGM2;
end
%X = [X,X(:,1)]; X = fliplr(X); % !! ANDERE ORIENTIERUNG !!
% -- Anfangsposition ------------------------
%PSI = - (5/6)*pi/2;
%cs = cos(PSI); ss = sin(PSI); DD = [cs, -ss; ss, cs];
%X  = DD*X;
START = 25;
X = [X(:,START:size(X,2)),X(:,1:START-1)];
%X = X(:,1:size(X,2)-1);
% -- Neues Zentrum ------------------------
SKALE = 0.4; % EINSTELLEN!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
M = [-(RB + R)-SKALE;0];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% -- Zentrum fuer Innenverzahnung
M = M + [3.5;0];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X = X + M*ones(1,size(X,2));
LR = [M(1)-FAKTOR*R; M(1)+FAKTOR*R];
UO = [M(2)-FAKTOR*R; M(2)+FAKTOR*R];
% -- Grafik -----------------------
axis([LR(1) LR(2) UO(1) UO(2)])
grid on, axis equal, axis manual, hold on
plot(X(1,:),X(2,:)), hold on
circle(M(1),M(2),0.05,'k')
circle(X(1,1),X(2,1),0.05,'k')

flag = 0;
if flag == 1
   for I = 1:size(X,2)
      plot(X(1,I),X(2,I),'.k'), hold on
      pause
   end
end

save scheibe18_b M R X
