function [p,e,segnr1,segnr2] = bsp02
% Rechteck mit Loch
%
% p sind die geordneten Knoten von Rand 1
% bzw. von Rand 1 und Rand 2
p1 = [...
0  0  ;1  0  ;2  0  ;3  0  ;4  0  ;5  0  ;
6  0  ;7  0  ;8  0  ;8  1  ;8  2  ;8  3  ;
8  4  ;8  5  ;8  6  ;8  7  ;8  8  ;7  8  ;
6  8  ;5  8  ;4  8  ;3  8  ;2  8  ;1  8  ;
0  8  ;0  7  ;0  6  ;0  5  ;0  4  ;0  3  ;
0  2  ;0  1  ];
p1 = p1';
%
N = 16; %Winkelunterteilung
J  = N:-1:1;
X  = [3 + 2*cos(2*pi*(J-1)/N)];
Y  = [3 + 2*sin(2*pi*(J-1)/N)];
p2 = [X;Y];
p  = [p1,p2];
e1 = [[1:31];[2:32]]; e1 = [e1,[32;1]];
e1 = [e1;zeros(2,32);ones(1,32)];

e2 = [[1:N-1],N;[2:N],1]; e2 = e2 +32;
e2 = [e2;zeros(2,N);2*ones(1,N)];
e  = [e1, e2];
segnr1 = 1; segnr2 = 2;
