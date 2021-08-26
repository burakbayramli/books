% Driver script for solving the 1D Maxwells equations using an ENO scheme
clear all

% Order of method
m=2;
    
% Set problem parameters
L = 2; FinalTime = pi/2; N = 256; CFL = 0.90; h = L/N; 
epl = 1.0; mul = 1.0; epr = 2.25; mur = 1.0;

% Define domain, materials and initial conditions
x = [-1:h:1]';
NGQ = 16; [xGQ,wGQ] = LegendreGQ(NGQ);
Ef = zeros(N+1,1); Hf = zeros(N+1,1);
for i=1:NGQ+1
    [Efh, Hfh, ep, mu] = CavityExact(x+xGQ(i)*h/2, epl, epr, mul, mur, 0);
    Ef = Ef + wGQ(i)*Efh; Hf = Hf + wGQ(i)*Hfh;
end
Ef = Ef/2; Hf = Hf/2;

% Solve Problem
q = [Ef Hf];
[q] = MaxwellENO1D(x,q,ep,mu,h,m,CFL,FinalTime);