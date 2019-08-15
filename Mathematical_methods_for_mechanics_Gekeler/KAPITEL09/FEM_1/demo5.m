function demo5
% Masterfile fuer elliptische Randwertprobleme
% kubische Dreieck- und Parallelogrammelemente

clear, clc, format short, format compact
% -- Parameter ---------------
RHO = 0;
Parmeter = RHO;
REFINE = 0;   % Anzahl Gitterverfeinerungen
% ------------------------------
Start = 100; KK = [0,1];
%while ~ismember(Start,KK)
%   Start = input('initialization yes/no ? (1/0) ');
%end
Start = 1;
if Start == 1
   %[p,e,t,q] = bsp003g;

   [p,e,t,q] = bsp001b(1);

   for I = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = mesh01(p,e,t);
   end
   save daten1 p e t q
end
load daten1 p e t q
[RD,RC,LASTEN] = bsp003h_k(p,e);
save daten2 RD RC LASTEN
Z = ellipt4(p,t,q,RD,RC,LASTEN,Parmeter);
% -- Ausgabe ------------------------------
N = size(p,2); Z1 = zeros(N,3);
for I = 1:N
   Z1(I,:) = [Z(3*(I-1)+1), Z(3*(I-1)+2), Z(3*(I-1)+3)];
end
Z2 = Z1(1:9,:);
save daten3 Z1
%disp(' "bild05" Aufrufen!  ');
bild05
