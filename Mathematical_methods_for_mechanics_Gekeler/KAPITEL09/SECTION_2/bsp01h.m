function [RD,RC,LOADS] = bsp01h(e)
% boundary data end CONSTANT right side f for example 9.2

% DIRICHLET boundary conditions  ----------
I  = find(e(5,:) == 5); LI = length(I);  % Boundary 5
RD = [e(1,I),e(2,I(LI));zeros(1,LI+1)];

% CAUCHY boundary conditions---------------
I    = find(e(5,:) == 2); LI = length(I); % Boundary 2
BETA = 3; GAMMA = 2;
RC2  = [e(1:2,I); BETA*ones(1,LI); GAMMA*ones(1,LI)];
BETA = 4; GAMMA = 1;
I    = find(e(5,:) == 4); LI = length(I); % Boundary 4
BETA = 4; GAMMA = 1;
RC4  = [e(1:2,I); BETA*ones(1,LI); GAMMA*ones(1,LI)];
RC   = [RC2,RC4];
LOADS = 10;
