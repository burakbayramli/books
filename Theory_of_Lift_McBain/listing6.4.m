x = [0, 1.25, 2.5:2.5:10, 15:5:25, 30:10:90, 95, 100]'/1e2;
[m, p] = naca4pars (2412);
y = naca4meanline (x, 0.02, 0.4);
[alpha0, Cmc4] = thin (x, y);
alpha0*180/pi, Cmc4
