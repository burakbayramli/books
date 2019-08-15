function [THETA1,THETA2,FALL] = grenzwinkel(X,Parmeter)
% Berechnet die Grenzwinkel THETA1 und THETA2
% Zeichnet V_EFF und DOT_PHI

FALL = 0;
[d3,D3,E] = invarianten(X,Parmeter)
E1   = E - D3^2/(2*T3);
T1   = Parmeter(1); T3   = Parmeter(2);
m    = Parmeter(3); gl   = Parmeter(4);
T1   = m*T1; T3 = m*T3;
if d3*d3 ~= D3*D3
   [THETA1,THETA2] = roots_f(a,b,alf,bet)
   if cos(THETA1) < d3/D3, FALL = 1; end
   if cos(THETA2) > d3/D3, Fall = 2; end
   if cos(THETA1) == d3/D3, FALL = 3; end
   if cos(THETA2) == d3/D3, FALL = 4; end
   if cos(THETA2) < d3/D3 & cos(THETA1) > d3/D3
      FALL = 5;
   end
   if THETA1 == THETA2, FALL = 6; end
end
if d3 == - D3
   FALL = 7;
   [THETA1,THETA2] = roots_f(a,b,alf,bet);
   % es muss THETA2 = pi sein
end
if d3 == D3 & D3 >= 2*sqrt(T1*m*gl)
   FALL = 8
   [THETA1,THETA2] = roots_f(a,b,alf,bet);
   % es muss THETA1 = 0 sein
end
if d3 == D3 & D3 < 2*sqrt(T1*m*gl)
   FALL = 9
   [THETA1,THETA2] = roots_f(a,b,alf,bet);
   % es muss THETA1 = 0 sein
end


   if cos(THETA2) < d3/D3 & cos(THETA1) > d3/D3
      FALL = 2;
   end
   if cos(THETA1) == d3/D3 | cos(THETA2) == d3/D3
      FALL = 3;
   end
   if THETA1 == THETA2
      FALL = 4;
   end
   Parmeter = [T1, T3, gl, m, tol];
   save daten T Y Parmeter THETA1 THETA2 d3 D3
   THETA1_THETA2_FALL = [THETA1, THETA2, FALL]
   bild05
end
if d3 == - D3 & D3 ~= 0
   THETA1 = []; THETA2 = pi;
   FALL = 5
   clf
   delta = 0.1;
   TT = linspace(delta,pi);
   V_EFF = (d3 - D3*cos(TT)).^2./(2*T1*sin(TT).^2)...
           + m*gl*cos(TT) + D3^2/(2*T3);
   plot(TT,V_EFF)
end
if d3 == D3 & D3 ~= 0
   if D3 < 0 | D3 > 2*sqrt(T1*m*gl)
      THETA1 = 0; THETA2 = [];
      FALL   = 6
      delta  = 0.1;
      TT     = linspace(delta,pi-2*delta,100);
      V_EFF  = (d3 - D3*cos(TT)).^2./(2*T1*sin(TT).^2)...
               + m*gl*cos(TT) + D3^2/(2*T3);
      plot(TT,V_EFF)
   end
   if 0 < D3 & D3 < 2*sqrt(T1*m*gl)
      THETA1 = 0; THETA2 = [];
      FALL = 7
      clf
      delta = 0.1;
      TT = linspace(delta,pi-delta,100);
      V_EFF = (d3 - D3*cos(TT)).^2./(2*T1*sin(TT).^2)...
              + m*gl*cos(TT) + D3^2/(2*T3);
      plot(TT,V_EFF)
   end
end
