function [p,e,segnr1,segnr2] = bsp07
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Kreis
% Die mit "1" gekennzeichneten Knoten sind der aussere Rand
% Die mit "2" gekennzeichneten Knoten sind der innere Rand
% Die Randknoten muessen durch Hinzunahme des ersten Randpunktes
% zu einem geschlossenen Polygon ergaenzt werden koennen

N = 32; %Winkelunterteilung
%
J = 1:N;
X = 5*cos(2*pi*(J-1)/N);
Y = sin(2*pi*(J-1)/N);
p = [X;Y];
e = [[1:N-1];[2:N]]; e = [e,[N;1]];
e = [e;zeros(2,N);ones(1,N)];
segnr1 = 1; segnr2 = [];
