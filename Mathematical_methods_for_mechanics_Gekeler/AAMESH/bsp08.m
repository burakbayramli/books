function [p,e,segnr1,segnr2] = bsp08
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Rand eines nichtkonvexen Gebietes
% Die mit "1" gekennzeichneten Knoten sind der aussere Rand
% Die mit "2" gekennzeichneten Knoten sind der innere Rand
% Die Randknoten muessen durch Hinzunahme des ersten Randpunktes
% zu einem geschlossenen Polygon ergaenzt werden koennen

N = 16; %Winkelunterteilung
%
J = [1:N];
X = cos(pi*(J-1)/N); Y = 2 - sin(pi*(J-1)/N);
p1 = [X;Y];
M = size(p1,2); p1 = [p1;ones(1,M)];
p1(3,2) = 0; p1(3,M) = 0;
K = find(p1(3,:) ~= 0); p1 = p1(1:2,K);

X = - ones(1,N); Y = 2 - 4*(J-1)/N;
p2 = [X;Y];
M = size(p2,2); p2 = [p2;ones(1,M)];
p2(3,2) = 0; p2(3,M) = 0;
K = find(p2(3,:) ~= 0); p2 = p2(1:2,K);

X = - cos(pi*(J-1)/N); Y = - 2 + sin(pi*(J-1)/N);
p3 = [X;Y];
M = size(p3,2); p3 = [p3;ones(1,M)];
p3(3,2) = 0; p3(3,M) = 0;
K = find(p3(3,:) ~= 0); p3 = p3(1:2,K);

X = ones(1,N); Y = - 2 + 4*(J-1)/N;
p4 = [X;Y];
M = size(p4,2); p4 = [p4;ones(1,M)];
p4(3,2) = 0; p4(3,M) = 0;
K = find(p4(3,:) ~= 0); p4 = p4(1:2,K);

p = [p1,p2,p3,p4];
M = size(p,2);
e = [[1:M-1];[2:M]]; e = [e,[M;1]];
e = [e;zeros(2,M);ones(1,M)];
segnr1 = 1; segnr2 = [];
