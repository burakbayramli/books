% Problem 4.2
M = 0.87;
p = 46500; % N/m/m
T = 273 - 24.6; % K
R = 286; 
Y = 1.4;
a = sqrt(Y*R*T)
rho = p/R/T; % kg/m/m/m
Cpm = -0.5;
dp =  Cpm*0.7*M^2*p 
pt = p+dp
ppo = pt/p/2
