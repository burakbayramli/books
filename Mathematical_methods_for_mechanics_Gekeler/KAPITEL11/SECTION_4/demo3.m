function demo3
% Abrollen von Scheibe A auf Scheibe B
% E.GEKELER, RELEASE 13.3.04
% Scheibe A : M1        : Mittelpunkt, X : Rand
% Scheibe B : M2 = (0,0): fester Mittelpunkt, Y : Rand
% anfaengl. Beruehrpunkt X(:,1) = Y(:,1)
% OPTION = 0: Scheibe A wandert um Scheibe B
% OPTION = 1: Scheibe A wandert auf und ab
% A_AUF_B = 1;  % 1 : A auf B; 0: A in B
% Zuerst Scheiben Konstruieren nach Vorlage
% Scheibe B positiv orientieren,
% Scheibe A positiv orientieren, wenn A in B l"auft,
% sonst negativ orientieren
% ACHTUNG: Polygonzug NICHT schliessen, weil
% doppelte Punkte nicht vorkommen duerfen
% TOL : Toleranz fuer Abbruch in Bisektion
% NN : Anzahl Winkelschritte, gekoppelt mit TOL
clc, clf, format compact, format short g
K = [1,2,3,4]; done = 0;
flag = 1;
if flag == 1
   while ~done
      nr   = input(' Beispiel Nr. (1/2/3/4) ')
      done = ismember(nr,K);
   end;
end
% -- ALLGEMEINE PARAMETER ---------------------
OPTION = 0;
% ------------------------------------------
%nr = 4;
switch nr
case 1
   % Zwei Zahnraeder, A auf B
   STEPNR  = 40;  % Anzahl Drehschritte
   TOL     = 1.0E-4; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   GRAFIK = 1; NN = 60; % Winkelschritt = 2*pi/NN
   load scheibe18 M R X, M1 = M; AUX = X;
   load scheibe17 M R X, M2 = M; Y = X; X = AUX;
   % -- Verschiebung ----------------------
   M1 = M1 - 0.05; X = X - 0.05;
case 2
   % Zwei Hundeknochen
   STEPNR  = 50;  % Anzahl Drehschritte
   TOL     = 1.0E-4; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   GRAFIK = 1; NN = 60; % Winkelschritt = 2*pi/NN
   load scheibe20 M R X, M1 = M; AUX = X;
   load scheibe19 M R X, M2 = M; Y = X; X = AUX;
   % -- Verschiebung --------------------
   M1 = M1 - 0.05; X = X - 0.05;
case 3
   % Zwei Zahnraeder, Innenverzahnung
   STEPNR  = 60;  % Anzahl Drehschritte
   TOL     = 1.0E-4; % Toleranz fuer Einfuegen
   A_AUF_B = 0;    % A auf/in B: 1/0
   GRAFIK = 1; NN = 60; % Winkelschritt = 2*pi/NN
   load scheibe18_b M R X, M1 = M; AUX = X;
   load scheibe17 M R X, M2 = M; Y = X; X = AUX;
case 4
   % Innenzahnradpumpe
   STEPNR  = 50;  % Anzahl Drehschritte
   TOL     = 1.0E-4; % Toleranz fuer Einfuegen
   A_AUF_B = 0;    % A auf/in B: 1/0
   GRAFIK = 1; NN = 60; % Winkelschritt = 2*pi/NN
   load scheibe24 M R X, M1 = M; AUX = X;
   load scheibe25 M R X, M2 = M; Y = X; X = AUX;
end

DPHI = 2*pi/NN;
WINKELWERTEA = []; WINKELA = 0;
WINKELWERTEB = []; WINKELB = 0;
ITER = 1; DONE = 0;
axis([-8 8 -8 8])
grid on, axis equal, axis manual, hold on
%STEPNR = 1;
while ~DONE
   ITER;
   Parmeter = [TOL;A_AUF_B;OPTION;DPHI;M1;M2;ITER];
   save daten M1 M2 X Y % LU OR
   bild01_b, pause(0.1)
   %FILM(ITER) = getframe;
   set(gca,'nextplot','replacechildren');
   cs = cos(DPHI); ss = sin(DPHI);
   if A_AUF_B == 0, ss = - ss; end
   DD = [cs, ss; -ss, cs];
   X1 = M1*ones(1,size(X,2)) + DD*(X - M1*ones(1,size(X,2)));
   [Y1,PSI,ECODE] = bisection(X1,Y,Parmeter);
   X = X1; Y = Y1;
   WINKELA      = WINKELA + DPHI;
   WINKELWERTEA = [WINKELWERTEA,WINKELA];
   WINKELB      = WINKELB + PSI;
   WINKELWERTEB = [WINKELWERTEB,WINKELB];
   ITER = ITER + 1;
   DONE = ITER > STEPNR | ECODE ==1;
end
save winkeldat WINKELWERTEA WINKELWERTEB
%save datenfilm FILM
%axis off
