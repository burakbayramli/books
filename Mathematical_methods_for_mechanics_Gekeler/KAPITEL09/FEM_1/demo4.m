 function demo4
% Masterfile fuer elliptische Randwertprobleme
% quadratische Dreieckselemente
% und quadratische Parallelogrammelemente
% Beispiel: Stationaere Waermeverteilung nach H.R. Schwarz
% FF1: File fuer erstes Gitter (entfaellt bei MATLAB TOOLBOX)
% FF2: File fuer Geometrie in MATLAB Format 
% FF3: File fuer Randbedingungen   

clear, clc, format short, format compact
% -- Parameter ---------------
FF1 = 'bsp001b'; FF2 = 'bsp001g'; FF3 = 'bsp003h_q';
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
   %[p,e,t,q] = bsp003g;
   [p,e,t,q] = feval(FF1,2);
   for I = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t,q] = mesh01_tq([],p,e,t,q);
   end
 %  bild04a(p,e,t,q)
 %  pause
   [p1,p2,e,t1,q1]  = mesh06_tq(p,e,t,q);
   save daten1 p e t q  p1 p2 t1 q1
end
load daten1 p e t q  p1 p2 t1 q1
[RD,RC,LASTEN] = feval(FF3,p,e);
save daten2 RD RC LASTEN
Z = ellipt3_t_q(p,t,q,p1,p2,t1,q1,RD,RC,LASTEN,Parmeter);
save daten3 Z
%disp(' "bild04" Aufrufen!  ');
bild04
