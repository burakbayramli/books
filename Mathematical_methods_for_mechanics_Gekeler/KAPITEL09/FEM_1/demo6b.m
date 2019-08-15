function demo6
% elliptische Randwertprobleme
% isopar.quadratische Dreieck- und Viereckelemente

clear, clc, format short, format compact
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
   [p,e,t,q] = bsp001b(2);

   for I = 1:REFINE
      disp(' Refinemesh ')
      %[p,e,t] = mesh01(p,e,t);
      [p,e,t,q] = mesh01_tq([],p,e,t,q);

   end
   [p1,p2,e,t1,q1]  = mesh06_tq(p,e,t,q);
   %[p1,e,t1]  = mesh06_t(p,e,t); p2 = []; q1 = [];
   save daten1 p e t q  p1 p2 t1 q1
end
load daten1 p e t q  p1 p2 t1 q1
[RD,RC,LASTEN] = bsp003h(p,e);
save daten2 RD RC LASTEN
Z = ellipt5(p,t,q,p1,p2,t1,q1,RD,RC,LASTEN,Parmeter);
save daten3 Z
%disp(' "bild06b" Aufrufen!  ');
bild06b
