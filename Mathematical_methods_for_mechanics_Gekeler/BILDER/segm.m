function [X,Y] = segm(A,B,PHI,FLAG)
% A,B Spaltenvektoren
% Zeichnet Kreissegment; Zentrum ist A
%clf
a = 0.02; % Radius fuer CIRCLE
r = 1/2;  %Radius fuer Punkt
P = B - A;
PSI = atan2(P(2),P(1));
LP = norm(P);
T = linspace(0,PHI,20);
X = A(1) + LP*cos(PSI+T);
Y = A(2) + LP*sin(PSI+T);
if FLAG == 1
X1 = A(1) + r*LP*cos(PSI+PHI/2);
Y1 = A(2) + r*LP*sin(PSI+PHI/2);
circle(X1,Y1,a,'k');
end
