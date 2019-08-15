function demo3
% Verschiebungen in raeumlichen Stabwerken
% E Elastizitaetsmodul
% F Querschnittsflaeche des Stabes
clear, clc, format compact, format short
disp('Verschiebungen in raeumlichen Stabwerken')
nr = 100; KK = [3, 4];
while ~ismember(nr,KK)
   nr   = input(' Beispiel Nr. (3/4) ');
end;
switch nr
case 3
   E = 0.2e9;    % Elastizitaetsmodul
   F = 5e-4;   % Querschnittsflaeche
   P = 30;       % Lastgewicht
   PARMETER = [E, F, P];
   [p,e,LAGER,LASTEN] = bsp03(PARMETER);
case 4
   E = 2.0e+07;
   F = 10;
   P = 0; % siehe BEISP04.M
   PARMETER = [E, F, P];
   [p,e,LAGER,LASTEN,TRI] = bsp04(PARMETER);
end

Z = stabwerk3(p,e,LAGER,LASTEN,PARMETER);
switch nr
case 3
   save daten3 Z p e LAGER LASTEN PARMETER
   fig0707
case 4
   save daten4 Z p e LAGER LASTEN PARMETER TRI
   fig0708b
end
ZZ = Z
% -- Berechnung der Spannungen  ---
STBX    = p(1,e(2,:)) - p(1,e(1,:));
STBY    = p(2,e(2,:)) - p(2,e(1,:));
STBZ    = p(3,e(2,:)) - p(3,e(1,:));
LA      = sqrt(STBX.*STBX+STBY.*STBY+STBZ.*STBZ);
P       = p + Z';
STBX    = P(1,e(2,:)) - P(1,e(1,:));
STBY    = P(2,e(2,:)) - P(2,e(1,:));
STBZ    = P(3,e(2,:)) - P(3,e(1,:));
LB      = sqrt(STBX.*STBX+STBY.*STBY+STBZ.*STBZ);
DIFF    = LA - LB;
SIGMA_U = E*DIFF';
