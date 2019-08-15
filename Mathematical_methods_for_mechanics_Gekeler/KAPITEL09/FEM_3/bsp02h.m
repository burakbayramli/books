function [RDZ,RDW] = bsp02h(e,Parmeter)
% flow past half cylinder,
% -- DIRICHLET BOUNDARY CONDITIONS FOR STREAM FUNCTION
VS = Parmeter(3);
I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDZ1 = [e(1,I(2:LI)); zeros(1,LI-1)];

I = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDZ2 = [e(1,I); zeros(1,LI)];

I = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDZ3 = [e(1,I); zeros(1,LI)];

I = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDZ5 = [e(1,I); 4*ones(1,LI)];

I = find(e(5,:) == 6); LI = length(I); % Randsegment 6, inlet
q = [e(1,I),e(2,I(LI))];
TT = linspace(5,0,LI+1); AUX = -0.016*TT.^3 + 0.24*TT.*TT;
RDZ6 = [e(1,I),e(2,I(LI)); AUX];

I = find(e(5,:) == 7); LI = length(I); % Randsegment 7
E = e(:,I); q = E(1,2:LI);
RDZ7 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];

I = find(e(5,:) == 9); LI = length(I); % Randsegment 9
RDZ9 = [e(1,I); 4*ones(1,LI)];

RDZ = [RDZ1, RDZ2, RDZ3, RDZ5, RDZ6, RDZ7, RDZ9];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cauchy boundary conditions for stream functions
% are used for artificial vorticity, see VS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- BOUNDARY CONDITIONS FOR VORTICITY
% TYP 1: w = w_B; TYP 2: w = w_wall;
% TYP 3: w = w_B in segments with w = w_wall;
% RD_W = [[Anf.-punkte]',TYP, Wert]

I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDW1 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];

I = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDW2 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];

I = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDW3 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];

I = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDW5 = [e(1,I); ones(1,LI); zeros(1,LI)];

I = find(e(5,:) == 6); LI = length(I); % Randsegment 6, inlet
TT = linspace(5,0,LI+1); AUX = 0.096*TT - 0.48;
RDW6 = [e(1,I),e(2,I(LI)); ones(1,LI+1); AUX];

I = find(e(5,:) == 7); LI = length(I); % Randsegment 7
RDW7 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];

I = find(e(5,:) == 9); LI = length(I); % Randsegment 9
RDW9 = [e(1,I); ones(1,LI); zeros(1,LI)];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Additional boundary conditions for W
% -- in segments with Wbound
I = find(e(5,:) == 2); LI = length(I);% Randsegment 2
RDW2(:,1) = [e(1,I(1)); 1; 0];
I = find(e(5,:) == 3); LI = length(I);% Randsegment 3
RDW3(:,1) = [e(1,I(1)); 1; 0];

RDW = [RDW1, RDW2, RDW3, RDW5, RDW6, RDW7, RDW9];
