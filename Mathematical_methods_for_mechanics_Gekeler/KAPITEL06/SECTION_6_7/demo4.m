function demo4
% Demo fuer Kreisel
% Zeichnet die Kurve des Euler-Winkels THETA
% und die Kurve von D_PHI
% Wenn man mit THETA1 oder THETA2 startet, muss
% D_THETA = 0 sein!
% Kurven haengt nicht von T3 ab
clc
% -- Parameter --------------
T1 = 1; T3 = 2; m = 1; gl = 2.9667/2; tol = 1.0E-3;
d3 = 3; D3 = 2;
THETA1 = pi/6; THETA2 = pi/2;
%---------------------
Parmeter = [T1, T3, gl, m, tol, d3, D3];
% Start -----------------
X0 = [THETA1;THETA2;0];
T_END = 6;
options = odeset('Reltol',1E-3,'Maxstep',0.01);
[T,Y]  = ode23(@bsp03,[0, T_END],X0,options,Parmeter);
%YY = Y(:,1)
save daten3 T Y Parmeter THETA1 THETA2 T_END
bild04
