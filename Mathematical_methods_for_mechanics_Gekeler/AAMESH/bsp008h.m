function [RD,RC,LASTEN] = bsp001h_cr(p,e,t)
% DIRICHLET-Randbedingungen ----------
I  = find(e(5,:) == 5); LI = length(I); % Boundary 5
RD = [e(6,I);zeros(1,LI)];
% ev. letzten Randpunkt dazunehmen

% CAUCHY-Randbedingungen ----------------
I    = find(e(5,:) == 2); LI = length(I); %Boundary 2
BETA = 3; GAMMA = 2;
RC2  = [e([1,2,6],I); BETA*ones(1,LI); GAMMA*ones(1,LI)];

I    = find(e(5,:) == 4); LI = length(I); % Boundary 4
BETA = 4; GAMMA = 1;
RC4  = [e([1,2,6],I); BETA*ones(1,LI); GAMMA*ones(1,LI)];
RC   = [RC2,RC4];
LASTEN = 10;
