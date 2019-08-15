function demo2
% Masterfile fuer elliptische Randwertprobleme
% lineare Parallelogramm-Elemente
% und Viereckelemente
% Beispiel: Einfaches Rechteck
% FF1: File fuer erstes Gitter (entfaellt bei MATLAB TOOLBOX)
% FF2: File fuer Geometrie in MATLAB Format 
% FF3: File fuer Randbedingungen   
% hier FF1 = FF2;
clear, clc, format short, format compact
FF1 = 'bsp002g'; FF3 = 'bsp002h';
% -- Parameter ---------------
RHO      = 1;
Parmeter = RHO;
REFINE = 1;   % Anzahl Gitterverfeinerungen
methode = 100;
while ~ismember(methode,[1,2])
   methode = input(' Parallel/Viereck? (1/2) ');
end
%methode = 2;
% ------------------------------
Start = 100; KK = [0,1];
%while ~ismember(Start,KK)
%   Start = input('initialization yes/no ? (1/0) ');
%end
Start = 1;
if Start == 1
   [p,e,t] = feval(FF1);
   for I = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = mesh01(p,e,t);
   end
   [RD,RC,LASTEN] = feval(FF3,p,e,t);
   save daten1 p e t
   save daten2 RD RC LASTEN
end
load daten1 p e t
load daten2 RD RC LASTEN
if methode == 1, disp(' Parallelogrammelemente ')
   Z   = ellipt2a(p,t,RD,RC,LASTEN,Parmeter);
end
if methode == 2, disp(' Viereckelemente ')
   Z   = ellipt2b(p,t,RD,RC,LASTEN,Parmeter);
end
save daten3 Z
%disp(' "bild02" Aufrufen!  ');
bild02
% -------------------------------------------------
