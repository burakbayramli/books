function demo1
% Verschiebungen in raeumlichen Balkenwerken
% E Elastizitaetsmodul, F Querschnittsflaeche
% RHO spezifisches Gewicht
clear, clc
nr = 100; KK = [1 2];
%while ~ismember(nr,KK)
%   nr   = input(' Beispiel Nr. (1/2) ');
%end;
nr = 1;
switch nr
case 1
   E   = 0.2e+08;
   NU  = 0.3;
   RHO = 1;
   PARMETER = [E, NU, RHO];
   [p,e,LAGER,LASTEN] = bsp01(PARMETER);
   Z = rahmen2(p,e,LAGER,LASTEN,PARMETER);
   %Z
end
DISPLACEMENTS = Z'
save daten6 Z p e LAGER LASTEN PARMETER
fig0709
