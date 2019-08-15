function test03
% Beispiel int_T x*y dxdy
clc, clear, format compact, format short

P = rand(1,6)*10;
X = P(1:3); Y = P(4:6);
F   = ((X(2)-X(1))*(Y(3)-Y(1)) - (X(3)-X(1))*(Y(2)-Y(1)))/2;
AA = [...
  1/2,  1/6,  1/6;
  1/6, 1/12, 1/24;
  1/6, 1/24, 1/12];
U = [X(1),X(2)-X(1),X(3)-X(1)];
V = [Y(1),Y(2)-Y(1),Y(3)-Y(1)];
DD = 2*F*U*AA*V.';
EE = prs(X,Y,1,1);
DIFF = DD - EE
       