function demo2
% MOVIE fuer Sieben_Koerper-Problem
clc
clf
clear
%set(gcf,'renderer','zbuffer')
set(gcf,'renderer','painter');
% -- konsistente Anfangswerte  -------------
X = zeros(7,1);
X(1) = - 0.0617138900142764496358;
X(2) =   0;
X(3) =   0.4552798191630703802559;
X(4) =   0.2226683901658855884674;
X(5) =   0.4873649795438425502255;
X(6) = - 0.2226683900165885884674;
X(7) =   1.2305474445498211924973;
Maxit   = 10;           % max. Schrittzahl im Newtonverfahren
tol     = 1E-3;        % Abbruchtoleranz
tol_i   = 1E-5;        % Abbruchtoleranz innere Iteration
n       = 6;
Parmtr1 = [Maxit,tol,tol_i,n]; % Parameter fuer Newtonverfahren
Parmtr2 = X(1);     % Parameter fuer Beispiel
N       = 60; % Anzahl Bilder
X0      = X(2:7);
%FILM = moviein(N);
axis([-0.09 0.03 -0.03 0.09])
axis equal
axis manual
grid on
hold on
bld060902a(X)
FILM(1) = getframe;
set(gca,'nextplot','replacechildren')
BETA = linspace(X(1),X(1)+2*pi,N) + X(1);
for I = 2:N
   beta = BETA(I);
   Parmtr2 = beta;
   [X1,errorcode] = newton('bsp02a',X0,Parmtr1,Parmtr2);
   Y = [beta;X1];
   X0 = X1;
   bld060802a(Y)
   FILM(I) = getframe;
   set(gca,'nextplot','replacechildren')
end
save datenfilm FILM
%pause
%load datenfilm FILM
%movie(FILM,5,2)

