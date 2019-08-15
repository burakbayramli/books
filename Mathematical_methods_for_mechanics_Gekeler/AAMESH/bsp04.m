function [p,e,segnr1,segnr2] = bsp04
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% p(1:2,:) : Randkoten, X-Komponenten, Y-Komponenten
% e(1:5,:) : Randkanten, uebliche Bedeutung der Zeilen
% Kreis aus einem Randsegment

N = 16; % Winkelunterteilung
J  = 1:N;
X = [cos(2*pi*(J-1)/N)]; Y = [sin(2*pi*(J-1)/N)];
p = [X;Y];
e = [[1:N-1];[2:N];zeros(1,N-1);ones(1,N-1);ones(1,N-1)];
e = [e,[N;1;0;1;1]];
segnr1 = 1; segnr2 = [];
