function demo1
% Beispiele zum Babuska-Paradoxon WITH PDE TOOLBOX
% Beisp. 1: w = (1-rr)*(5 - rr), rr = x^2 + y^2
% Beisp. 2: w = (1 - 2*rr)*(5 - rr), phi = atan(y/x);
% Beisp. 3: w = (rr^3 - 2*rr^2 - 4*rr)*cos(2*phi))/10;
% Beisp. 4: w = (2*rr^3 - 5*rr^2)*cos(2*phi))/12;
% w_n numerisch am Rand
% RANDV.M:   Randbedingung fuer v
% BSPXXHW.M: Randbedingung fuer w

clear, clc, format short, format compact
Beispiel = 100;
while ~ismember(Beispiel,[1,2,3,4])
   Beispiel = input('Beispiel Nr. = ? (1/2/3/4) ');
end
% Fuer die Anwendung muessen die folgenden Variablen
% global deklariert werden:

global nu, global t, global y1, global y2
switch Beispiel
case 1
   [p,e,t] = bsp01g;       % Geometrie
   MAXIT   = 10;           % Max. Anzahl Iterationen
   TOL     = 1E-2;         % Display maximaler Fehler
   nu      = 0;            % Poissonzahl
   FF      = 'bsp01hw';    % Randbedingung fuer w
   w_exact = bsp01(p,e,1); % Exakte Loesung
   F       = bsp01(p,e,2); % Rechte Seite
   y1      = bsp01(p,e,3); % 2. Abl. von p (Buch) in e(1,:)
   y2      = bsp01(p,e,4); % 2. Abl. von p (Buch) in e(2,:)
   F_V     = pdeintrp(p,t,F);
case 2
   [p,e,t] = bsp01g;
   MAXIT   = 10; TOL = 1E-2; nu = 1/7;
   FF      = 'bsp02hw';
   w_exact = bsp02(p,e,1);
   F       = bsp02(p,e,2);
   y1      = bsp02(p,e,3);
   y2      = bsp02(p,e,4);
   F_V     = pdeintrp(p,t,F);
case 3
   [p,e,t] = bsp01g;
   MAXIT   = 10; TOL = 1E-3; nu = 1/5;
   FF      = 'bsp03hw';
   w_exact = bsp03(p,e,1);
   F       = bsp03(p,e,2);
   y1      = bsp03(p,e,3);
   y2      = bsp03(p,e,4);
   F_V     = pdeintrp(p,t,F);
case 4
   [p,e,t] = bsp01g;
   MAXIT   = 10; TOL = 1E-3; nu = 0;
   FF      = 'bsp04hw';
   w_exact = bsp04(p,e,1);
   F       = bsp04(p,e,2);
   y1      = bsp04(p,e,3);
   y2      = bsp04(p,e,4);
   F_V     = pdeintrp(p,t,F);
end
w = zeros(size(p,2),1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for I = 1:MAXIT
   w_aux = w; % notwendig
   v     = assempde('randv',p,e,t,1,0,F_V,w);
   F_W   = pdeintrp(p,t,v);
   w     = assempde(FF,p,e,t,1,0,F_W);
   error = norm(w-w_exact,inf);
   iter_error = [I,error]
end
diff = abs(w-w_exact);
J    = find(abs(diff - error) < TOL);
save daten p t e w J w_exact
%disp(' bild01 aufrufen! ')
bild01
clear
