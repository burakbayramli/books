function demo1
% Verschiebungen beim Biegebalken in spezieller Lage
% mit Einzellasten und segmentweise konstanten Lastdichten
clear, clc, format compact, format long
%nr = 100; KK = [1 2];
%while ~ismember(nr,KK)
%   nr   = input(' Beispiel Nr. (1/2) ');
%end;
nr = 1;
switch nr
case 1 % Laengen in [cm]
   [p,e,Lager,Lasten,Lastdichten,Parmeter] = bsp01;
   [Z,ecode] = balken1(p,e,Lager,Lasten,Lastdichten,Parmeter);
   save daten4 Z p e Lager Lasten Lastdichten Parmeter
   fig0704
case 2
   [p,e,Lager,Lasten,Parmeter] = bsp05;
   Z = rahmen2(p,e,Lager,Lasten,Parmeter);
   Z
end
