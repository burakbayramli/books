function [d3,D3,E] = invarianten(X,Parmeter)
% berechnet die Invarianten aus den Startwerten X
% X = [PHI,THETA,PSI,D_PHI,D_THETA,D_PSI];
% Ergebnis haengt nicht von X(3) = PSI ab
T1     = Parmeter(1); T3   = Parmeter(2);
m      = Parmeter(3); gl   = Parmeter(4);
T1     = m*T1; T3 = m*T3;
d3   = X(4)*(T1*sin(X(2))^2 + T3*cos(X(2))^2)+X(6)*T3*cos(X(2));
D3   = T3*(X(6) + X(4)*T3*cos(X(2)));
V_EFF = (d3 - D3*cos(X(2)))^2/(2*T1*sin(X(2))^2) + m*gl*cos(X(2))...
        + D3^2/(2*T3);
E   = 0.5*T1*X(5)^2 + V_EFF;
