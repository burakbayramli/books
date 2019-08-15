function [C,D] = trilin2_aux(X,Y,W,Z);
% liefert die Matrix C(4,4) fuer die trilineare Abbildung
% gerade bilineare Parallelogrammelemente
% INPUT:
%     X,Y die Eckenkoordinatenpaare
%     W Werte der Rotation an den Knotenpunkten
%     Z Werte der Stromfunktion an den Knotenpunkten
% OUTPUT:
%     C Matrix C(Z) der Trilinearform c_e(Z,U,V) = U'*C(Z)*W
%     D = GRADIENT(C(Z)*V) = GRAD(C(Z))*V + C(Z)

C    = sparse(4,4);
X21  = X(2) - X(1); X41  = X(4) - X(1);
Y21  = Y(2) - Y(1); Y41  = Y(4) - Y(1);
DET  = 360*(X21*Y41 - X41*Y21);
C1   = [ 90, 30, 30; 30, 15, 10; 30, 10, 15]/DET;
C2   = [ 90, 60, 30; 60, 45, 20; 30, 20, 15]/DET;
C3   = [ 90, 30, 60; 30, 15, 20; 60, 20, 45]/DET;
C4   = [ 90, 60, 60; 60, 45, 40; 60, 40, 45]/DET;
a = Y41; b = - Y21;
E = ...
[-a-b,  a, 0,  b;
    b, -b, b, -b;
    a, -a, a, -a];
a = - X41; b = X21;
F = ...
[-a-b,  a, 0,  b;
    b, -b, b, -b;
    a, -a, a, -a];
G = (E*Z)'; H = (F*Z)';
C(1,:) = H*C1*E - G*C1*F;
C(2,:) = H*C2*E - G*C2*F;
C(3,:) = H*C3*E - G*C3*F;
C(4,:) = H*C4*E - G*C4*F;

G = E'; H = F';
D(:,1) = (H*C1*E - G*C1*F)*W;
D(:,2) = (H*C2*E - G*C2*F)*W;
D(:,3) = (H*C3*E - G*C3*F)*W;
D(:,4) = (H*C4*E - G*C4*F)*W;
D      = D';
