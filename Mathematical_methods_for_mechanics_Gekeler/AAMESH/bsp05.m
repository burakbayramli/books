function [p,e,segnr1,segnr2] = bsp05
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% vierblaettriges Kleeblatt mit vier Randsegmenten
% p(1:2,:) : Randkoten, X-Komponenten, Y-Komponenten
% e(1:5,:) : Randkanten, uebliche Bedeutung der Zeilen
N = 8; %Winkelunterteilung
%
J  = 1:N;
a  = 1; b  = 0; c  = - pi/2;
X1 = [a + cos(c + pi*(J-1)/N)];
Y1 = [b + sin(c + pi*(J-1)/N)];
p1 = [X1;Y1];
e1 = [[1:N];[2:N+1];zeros(1,N);ones(1,N);ones(1,N)];
% ------------------------------
a  = 0; b  = 1; c  = 0;
X2 = [a + cos(c + pi*(J-1)/N)];
Y2 = [b + sin(c + pi*(J-1)/N)];
p2 = [X2;Y2];
e2 = [N+[1:N];N+[2:N+1];zeros(1,N);ones(1,N);2*ones(1,N)];
% ------------------------------
a  = -1; b  =  0; c  = pi/2;
X3 = [a + cos(c + pi*(J-1)/N)];
Y3 = [b + sin(c + pi*(J-1)/N)];
p3 = [X3;Y3];
e3= [2*N+[1:N];2*N+[2:N+1];zeros(1,N);ones(1,N); 3*ones(1,N)];
% ------------------------------
a  = 0; b  = - 1; c  = -pi;
X4 = [a + cos(c + pi*(J-1)/N)];
Y4 = [b + sin(c + pi*(J-1)/N)];
p4 = [X4;Y4];
e4 = [3*N+[1:N];3*N+[2:N+1];zeros(1,N);ones(1,N);4*ones(1,N)];
e4(2,N) = 1;
p = [p1,p2,p3,p4];
e = [e1,e2,e3,e4];
segnr1 = [1,2,3,4]; segnr2 = [];
