function [FLAG,X,ecode] = schnitt(A,B,C,D);
% GEKELER: FINITE ELEMENTE --------------------------
% untersucht, ob sich zwei Strecken schneiden, wenn ja
% dann S = A + X(1)*(B - A) = C + X(2)*(D - C);
ecode = 0; tol = 1.0E-8;
E = [B-A, C-D]; F = C-A; X = [-1;-1];
if abs(det(E)) > tol
   X = E\F;
else
   ecode = 2;
   if abs(F(1)- F(2)) < tol; ecode = 1; end
end
FLAG = 0;
if X >= -100*eps & X <= 1+100*eps, FLAG = 1; end
