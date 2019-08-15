function demo2
% Abrollen von Scheibe A auf Scheibe B
% E.GEKELER, RELEASE 13.3.04
% Scheibe A : M1        : Mittelpunkt, X : Rand
% Scheibe B : M2 = (0,0): fester Mittelpunkt, Y : Rand
% anfaengl. Beruehrpunkt X(:,1) = Y(:,1)
% NN1 : Anzahl Suchpunkte auf Scheibe A : Einstellen!
% NN2 : Anzahl Suchpunkte auf Scheibe B : Einstellen!
% NN1, NN2 gemaess Feinheitsgrad WAEHLEN!
% OPTION = 0: Scheibe A wandert um Scheibe B
% OPTION = 1: Scheibe A wandert auf und ab
% Zuerst Scheiben Konstruieren nach Vorlage
% Scheibe B positiv orientieren,
% Scheibe A positiv orientieren, wenn A in B laeauft,
% sonst negativ orientieren
% ACHTUNG: Polygonzug NICHT schliessen, weil
% doppelte Punkte nicht vorkommen duerfen

clear, clc, format compact, format short g
nr = 100; KK = [1,2,3,4,5,6,7,8,9,10,11];
while ~ismember(nr,KK)
   nr   = input(' Beispiel Nr. (1/2/3/4/5/6/7/8/9/10/11) ')
end;
% -- ALLGEMEINE PARAMETER ---------------------
OPTION = 0;
DREHMAX = 0.8; %COS des maximalen Drehwinkels
% ------------------------------------------
%nr = 4
switch nr
case 1
   STEPNR  = 82;  % Anzahl Drehschritte
   TOL     = 0.01; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   NN1 = 3; NN2 = 5; GRAFIK = 1;
   load scheibe01 M R X, M1 = M; AUX = X;
   load scheibe03 M R X, M2 = M; Y = X; X = AUX;
case 2
   STEPNR  = 96;  % Anzahl Drehschritte
   TOL     = 0.01; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   NN1 = 3; NN2 = 10; GRAFIK = 1;
   load scheibe02 M R X, M1 = M; AUX = X;
   load scheibe04 M R X, M2 = M; Y = X; X = AUX;
case 3
   STEPNR  = 120;  % Anzahl Drehschritte
   TOL     = 0.005; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   NN1 = 2; NN2 = 10; GRAFIK = 1;
   load scheibe06 M R X, M1 = M; AUX = X;
   load scheibe04 M R X, M2 = M; Y = X; X = AUX;
case 4
   STEPNR  = 129;  % Anzahl Drehschritte
   TOL     = 0.01; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   NN1 = 4; NN2 = 10; GRAFIK = 1;
   load scheibe01 M R X, M1 = M; AUX = X;
   load scheibe07 M R X, M2 = M; Y = X; X = AUX;
case 5
   STEPNR  = 110;  % Anzahl Drehschritte
   TOL     = 0.01; % Toleranz fuer Einfuegen
   A_AUF_B = 0;    % A auf/in B: 1/0
   NN1 = 8; NN2 = 10; GRAFIK = 1;
   load scheibe08 M R X, M1 = M; AUX = X;
   load scheibe07 M R X, M2 = M; Y = X; X = AUX;
case 6
   STEPNR  = 10;   % Anzahl Drehschritte
   TOL     = 0.02; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   NN1 = 7; NN2 = 10; GRAFIK = 1;
   load scheibe10 M R X, M1 = M; AUX = X;
   load scheibe11 M R X, M2 = M; Y = X; X = AUX;
case 7
   STEPNR  = 93;  % Anzahl Drehschritte
   TOL     = 0.001; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   NN1 = 10; NN2 = 24; GRAFIK = 2;
   load scheibe15 M R X, M1 = M; AUX = X;
   load scheibe05 M R X, M2 = M; Y = X; X = AUX;
case 8
   STEPNR  = 148;  % Anzahl Drehschritte
   TOL     = 0.01; % Toleranz fuer Einfuegen
   A_AUF_B = 0;    % A auf/in B: 1/0
   NN1 = 16; NN2 = 18; GRAFIK = 1;
   load scheibe16 M R X, M1 = M; AUX = X;
   load scheibe07 M R X, M2 = M; Y = X; X = AUX;
case 9
   STEPNR  = 45;  % Anzahl Drehschritte
   TOL     = 0.01; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   NN1 = 4; NN2 = 8; GRAFIK = 1;
   load scheibe18 M R X, M1 = M; AUX = X;
   load scheibe17 M R X, M2 = M; Y = X; X = AUX;
case 10
   STEPNR  = 58;  % Anzahl Drehschritte
   TOL     = 0.01; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   NN1 = 4; NN2 = 8; GRAFIK = 1;
   load scheibe20 M R X, M1 = M; AUX = X;
   load scheibe19 M R X, M2 = M; Y = X; X = AUX;
case 11
   STEPNR  = 140;  % Anzahl Drehschritte
   TOL     = 0.01; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   NN1 = 8; NN2 = 8; GRAFIK = 1;
   load scheibe01 M R X, M1 = M; AUX = X;
   load scheibe22 M R X, M2 = M; Y = X; X = AUX;
end
%STEPNR = 1;
axis([-8 8 -8 8])
grid on, axis equal, axis manual, hold on
% -- KONSTRUKTION -------------------------------
X1 = X; Y1 = Y; PP = X(:,1); % erster Drehpunkt
X1 = [X1;ones(1,size(X1,2))]; Y1 = [Y1;ones(1,size(Y1,2))];
CENTER = M1;
for ITER = 1:STEPNR
   ITER;
   Parmeter = [NN1,NN2,TOL,A_AUF_B,OPTION,DREHMAX,ITER];
   save daten M1 M2 X Y CENTER% LU OR
   if GRAFIK == 1, bild01, pause(0.1),   end
   if GRAFIK == 2, bild01_a, pause(0.1), end
   %FILM(ITER) = getframe;
   set(gca,'nextplot','replacechildren');
   [M1N,M2N,XN,YN,X1N,Y1N] = ...
                disc_rot_neu(M1,M2,X,Y,X1,Y1,PP,Parmeter);
   M1 = M1N; M2 = M2N; X = XN; Y = YN; X1 = X1N; Y1 = Y1N;
   PP = X1(1:2,1);
   CENTER = [CENTER,M1];
end
%axis off
%save datenfilm FILM
save datenaux X Y X1 Y1
