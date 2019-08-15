function demo7
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
clear, clc, format compact
disp(' Normalen und Nachbarn fuer Navier-stokes-gleichungen ')
disp(' Stromfunktion-Wirbel-Form ')
ecode = 0; beispnr = 100; KK = [1,2,3,4,5];
while ~ismember(beispnr,KK)
   beispnr = input(' Welches Beispiel?, (1/2/3/4/5) ');
end;
%beispnr = 5
switch beispnr
case 1, disp(' Figure for lid driven cavity ')
   FF1    = 'bsp01g';
   LAENGE = 0.3; % Geometriedaten
   FAKTOR = 0.2; % Scaling for normals in image
   REFINE = 3;   % Anzahl Gitterverfeinerungen
   DIST   = 0.2; % Abstand zum Bildrand
   SEGNR  = [1,2,3,4]; % Rand gesamt (geordnet)
   segnr1 = SEGNR;     % Aussenrand  (geordnet)
   segnr2 = [];        % Innenrand   (geordnet)
case 2, disp(' Figure for flow past half cylinder ')
   FF1    = 'bsp02g';
   LAENGE = 2; % Length of normals in bound for SCHNITT.M
   FAKTOR = 1; % Scaling for normals in image
   REFINE = 3; % Number of uniform refinements
   DIST   = 1; % Abstand zum Bildrand
   SEGNR  = [1,2,3,7,8,9,5,6];
   segnr1 = SEGNR; segnr2 = [];
case 3, disp(' Figure for back facing step ')
   FF1    = 'bsp04g';
   LAENGE = 0.015; %Length of normals in bound for SCHNITT.M
   FAKTOR = 0.015; % Scaling for normals in image
   REFINE = 2;     % Number of uniform refinements; = 3 in Beispiel
   DIST   = 0.01;  % Abstand zum Bildrand
   SEGNR  = [6 7 8 9 14 19 24 29 34 39 44 45 42 43 38 33 28 23 18 13 3 5];
   segnr1 = SEGNR; segnr2 = [];
case 4, disp(' Figure for transport ')
   FF1    = 'bsp05g';
   LAENGE = 16; %Length of normals in boundary for SCHNITT.M
   FAKTOR = 16; % Scaling for normals in image
   REFINE = 0; % Number of uniform refinements
   DIST   = 2;  % Abstand zum Bildrand
   SEGNR  = [1 2 3 4 5 6];
   segnr1 = SEGNR;  segnr2 = [];
case 5
   disp('Figure for flow past cylinder ')
   FF1    = 'bsp03g';
   LAENGE = 1.5; % Length of normals in bound for SCHNITT.M
   FAKTOR = 1; % Scaling for normals in image
   REFINE = 3; % Number of uniform refinements
   DIST   = 1; % Abstand zum Bildrand
   SEGNR  = [1,7,8,9,3,4,5,6];
   segnr1 = [1,7,8,9,3,4]; segnr2 = [5,6];
end
% -- Computation of offset data
if beispnr == 4, [p,e,t] = bsp05g; else
[p,e,t,RAND,INNERPKTE]         = prepar(FF1,REFINE,SEGNR);
end
[RDKN,RDEL,OFFSET]             = mesh40(p,e,segnr1,t);
[NACHBARN,NORMALEN,ecode]      = mesh43(p,e,segnr1,OFFSET,LAENGE);
if ~isempty(segnr2)
   [RDKN2,RDEL2,OFFSET2]       = mesh40(p,e,segnr2,t);
   [NACHBARN2,NORMALEN2,ecode] = mesh43(p,e,segnr2,OFFSET2,LAENGE);
   [M1,N1] = size(RDKN); [M2,N2] = size(RDKN2);
   if M1 > M2, RDKN2 = [zeros(M1-M2,N2);RDKN2]; end
   if M2 < M1, RDKN = [zeros(M2-M1,N1);RDKN]; end
   RDKN = [RDKN,RDKN2]; RDEL = [RDEL;RDEL2];
   NACHBARN = [NACHBARN,NACHBARN2];
   NORMALEN = [NORMALEN,NORMALEN2]; OFFSET = [OFFSET,OFFSET2];
end
save daten p e t NORMALEN NACHBARN OFFSET segnr1 segnr2 DIST FAKTOR RDEL RDKN
disp(' bild03 Aufrufen ! ')
%bild03
clear
