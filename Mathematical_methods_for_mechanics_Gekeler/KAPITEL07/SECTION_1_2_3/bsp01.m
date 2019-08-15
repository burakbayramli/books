function [p,e,Lager,Lasten,Lastdichten,Parmeter] = bsp01
% einfacher Balken nach SCHWARZ80
E   = 2.0e7;  % Elastizitaetsmodul [N/cm^2]
I   = 16;      % Fl.-traegheitsmoment [cm^4]
P   = 100;    % Einzellast an Knoten 2
f   = 2;      % kont. Last in Segment 3 [N/cm]
Parmeter = [E,I];

p  = [0, 150, 200, 300];
% e = Balkenelemente, Reihenfolge von links nach rechts
% 1. Zeile: Anfangspunkte, 2. Zeile: Endpunkte
% 3. Zeile: Balkenhoehe,   4. Zeile: Balkenbreite
% 5. Zeile: spez. Gewicht
e = [1, 2, 3;
     2, 3, 4;
     0, 0, 2;
     0, 0, 2;
     0, 0, 0]; % Eigengewicht vernachlaessigt, RHO = 1 in I;
% Lager = [U; U']; Eins: U = 0, Null: U nicht vorgegeben,
%                  ebenso U'
Lager = [1, 0, 1, 1;
         1, 0, 0, 0];
Lasten  = [0,-P,0,0;   % in Trennpunkten 1. Zeile Last, 2. Zeile Moment
           0,0,0,0];
Lastdichten = [0,0,-f]; %in Balkenelementen

