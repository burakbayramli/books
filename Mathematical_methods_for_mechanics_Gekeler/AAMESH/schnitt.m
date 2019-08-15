function [FLAG,X] = schnitt(A,B,C,D);
% GEKELER: FINITE ELEMENTE --------------------------
% untersucht, ob sich zwei Strecken schneiden, wenn ja
% dann S = A + X(1)*(B - A) = C + X(2)*(D - C);
TOL = 100*eps;
E    = [B-A, C-D];
F    = C-A;
X    = [0;0];
if abs(det(E)) > 1.0e-8
   X = E\F;
end
FLAG = 0;
if min(X) > TOL & max(X) < 1-TOL
   FLAG = 1;
end
