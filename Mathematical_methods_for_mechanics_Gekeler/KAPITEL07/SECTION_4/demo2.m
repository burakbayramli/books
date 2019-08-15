function demo2
% Verschiebungen in ebenen Stabwerken
% E Elastizitaetsmodul
% F Querschnittsflaeche des Stabes
clear, clc, format compact, format short
nr = 100; KK = [1, 2];
while ~ismember(nr,KK)
   nr   = input(' Beispiel Nr. (1/2) ');
end;
switch nr
case 1
   E   = 0.2E9;   % : Elastizitaetsmodul
   F   = 0.5E-3;  % : Flaeche
   PHI = pi/12;   % : Lagerwinkel in Knoten 5
   P   = 10;      % : Lastgewicht
   PARMETER = [E, F, P, PHI];
   [p,e,LAGER,LASTEN] = bsp01(PARMETER);
case 2
   E   = 0.2E9;   % : Elastizitaetsmodul
   F   = 0.5E-3;  % : Flaeche
   PHI = pi/12;   % : Lagerwinkel in Knoten 5
   P   = 10;      % : Lastgewicht
   PARMETER = [E, F, P, PHI];
   [p,e,LAGER,LASTEN] = bsp02(PARMETER);
end
Z = stabwerk2(p,e,LAGER,LASTEN,E,F);
save daten2 Z p e LAGER LASTEN PARMETER nr
%Z
% -- Berechnung der Spannungen  ---
STB_X   = p(1,e(2,:)) - p(1,e(1,:));
STB_Y   = p(2,e(2,:)) - p(2,e(1,:));
LA      = sqrt(STB_X.*STB_X + STB_Y.*STB_Y);
P       = p + Z';
STB_X   = P(1,e(2,:)) - P(1,e(1,:));
STB_Y   = P(2,e(2,:)) - P(2,e(1,:));
LB      = sqrt(STB_X.*STB_X + STB_Y.*STB_Y);
DIFF    = LA - LB;
SIGMA_U = E*DIFF'
bild02
