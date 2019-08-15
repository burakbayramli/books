function demo7
% Beispiel: Stationaere Waermeverteilung nach H.R. Schwarz, S. 185,186
% Crouzeix-Raviart-Elemente
% Lineare Dreieckselemente mit Knotenpunkten
% in den Seitenmitten
% FF1: File fuer erstes Gitter (entfaellt bei MATLAB TOOLBOX)
% FF2: File fuer Geometrie in MATLAB Format 
% FF3: File fuer Randbedingungen   

clear, clc, format short, format compact
% Beispiel:
FF1 = 'bsp001b'; FF2 = 'bsp001g'; FF3 = 'bsp001h_cr';
% -- Parameter ---------------
RHO = 0;
Parmeter = RHO;
REFINE = 1;   % Anzahl Gitterverfeinerungen
% ------------------------------
Start = 100; KK = [0,1];
%while ~ismember(Start,KK)
%   Start = input('initialization yes/no ? (1/0) ');
%end
Start = 1;
if Start == 1
   [p,e,t,q] = feval(FF1,1);
   for I = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = mesh01_t(FF2,p,e,t);
   end
   disp(' Zwischenpunkte Einfuegen ! ')
   [p1,e,t1]  = mesh06_t(p,e,t); 
   save daten1 p e t p1 t1
end
load daten1 p e t p1 t1
[RD,RC,LASTEN] = feval(FF3,p,e);
save daten2 RD RC LASTEN
Z1 = ellipt_cr(p,t,p1,t1,RD,RC,LASTEN,Parmeter);
Z1 = Z1';
xlin  = linspace(min(p1(1,:)),max(p1(1,:)),20);
ylin  = linspace(min(p1(2,:)),max(p1(2,:)),20);
[U,V] = meshgrid(xlin,ylin);
W     = griddata(p1(1,:),p1(2,:),Z1,U,V);
Z = interp2(U,V,W,p(1,:),p(2,:));
save daten3 Z
%disp(' "bild07" Aufrufen!  ');
bild07
