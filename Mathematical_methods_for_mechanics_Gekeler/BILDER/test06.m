% Rechnung fuer Kepler-Problem
syms mk D mE  u
f = acos((D*D*u - mk)/sqrt(mk*mk +2*mE*D*D));
g = diff(f,u);
h = simple(g)


