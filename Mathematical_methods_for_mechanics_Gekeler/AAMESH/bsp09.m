function [p,e,segnr1,segnr2] = bsp09
% Ring
%
% p sind die geordneten Knoten von Rand 1
% bzw. von Rand 1 und Rand 2
N = 16; %Winkelunterteilung
J  = 1:N;
X  = cos(2*pi*(J-1)/N);
Y  = sin(2*pi*(J-1)/N);
p1 = [X;Y];
%
N = 16; %Winkelunterteilung
J  = N:-1:1;
X  = 0.5*cos(2*pi*(J-1)/N);
Y  = 0.5*sin(2*pi*(J-1)/N);
p2 = [X;Y];
p  = [p1,p2];
e1 = [[1:N-1];[2:N]]; e1 = [e1,[N;1]];
e1 = [e1;zeros(2,N);ones(1,N)];

e2 = [[1:N-1];[2:N]]; e2 = [e2,[N;1]]; e2 = e2 +16;
e2 = [e2;zeros(2,N);2*ones(1,N)];
e  = [e1, e2];
segnr1 = 1; segnr2 = 2;
