function [U,F] = tstfkt1(KNOTEN,RHO)
% Elliptische RWP, Testfunktion
% Berechnet fuer ein Quadrat mit der Seitenlaenge 1 die Fkt.
% U = sin^2(PI*X)*sin^2(PI*Y) und die RS
% F = U_XX + U_YY - RHO*U.

X    = KNOTEN(:,2);
Y    = KNOTEN(:,3);
U    =  sin(pi*X).^2.*sin(pi*Y).^2;
U_XX =  2*pi^2*(cos(pi*X).^2 - sin(pi*X).^2).*sin(pi*Y).^2;
U_YY =  2*pi^2*(cos(pi*Y).^2 - sin(pi*Y).^2).*sin(pi*X).^2;
F    = U_XX + U_YY - RHO*U;
