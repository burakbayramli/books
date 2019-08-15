function [RD,RC,LOADS] = bsp001h_tst(p,e,t)
% example with exact solution u = (x-5)^2 + y^2
% no boundary condition in a segment means that 
% u_n := gradu*n = 0 IN THE LIMIT !! 

% DIRICHLET boundary conditions  ----------
I  = find(e(5,:) == 2); LI = length(I); % Boundary 2
NN = [e(1,I(1)),e(2,I)];  % Node numbers on boundary 
RD2 = [NN; (p(1,NN) - 5).^2 + p(2,NN).^2];

I  = find(e(5,:) == 4); LI = length(I); % Boundary 4
NN = e(1,I);
RD4 = [NN; (p(1,NN) - 5).^2 + p(2,NN).^2];

I  = find(e(5,:) == 5); LI = length(I); % Boundary 5
NN = [e(1,I(1)),e(2,I)];  % Node numbers on boundary 
RD5 = [NN; (p(1,NN) - 5).^2 + p(2,NN).^2];

RD = [RD2,RD4,RD5]; % Dirichlet boundary conditions
RC = [];            % Cauchy or third boundary conditions 
LOADS = -4;
