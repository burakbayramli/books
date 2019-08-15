function demo1
% Kraefte in ebenen Stabwerken

clear, clc, format compact, format short
nr = 100; KK = [1,2];
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
X = stabwerk1(p,e,LAGER,LASTEN);
N = size(e,2); M = size(LAGER,2);
STABKRAEFTE  = -X(1:N);        % Neg. Vorzeichen !!!
LAGERKRAEFTE = [LAGER(1,:);LAGER(2:3,:)*diag(X(N+1:N+M))];
KRAEFTE      = [[1:N]',STABKRAEFTE/PARMETER(3)]
SIGMA_K      = -STABKRAEFTE/PARMETER(2)
save daten1 p e STABKRAEFTE LAGERKRAEFTE LASTEN PARMETER
fig0705_06
