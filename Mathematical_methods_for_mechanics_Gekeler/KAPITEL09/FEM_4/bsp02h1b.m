function [RDZ,RCZ,RDW,RDT,RCT] = bsp02h1(p,e,t,Parmeter)
% convection in a closed compartment
% -- BOUNDARY CONDITIONS FOR STREAM FUNCTION
TEMPRECHTS = 1/2;
TEMPLINKS = -1/2;

VS = Parmeter(3);
I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDZ_1 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDZ_2 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDZ_3 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDZ_4 = [e(1,I); zeros(1,LI)];
RDZ = [RDZ_1, RDZ_2, RDZ_3, RDZ_4];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
RCZ = [];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- BOUNDARY CONDITIONS FOR VORTICITY
% TYP 1: w = w_B; TYP 2: w = w_wall;
% TYP 3: w = w_b in segments with w = w_wall
% RD_W = [[Anf.-punkte]',TYP, Wert]

I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDW_1 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDW_2 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDW_3 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];
I  = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDW_4 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];

% -- Additional boundary conditions for W
% -- in segments with Wbound
I  = find(e(5,:) == 1); % Randsegment 1
RDW_1(:,1) = [e(1,I(1)); 1; 0];
I  = find(e(5,:) == 2); % Randsegment 2
RDW_2(:,1) = [e(1,I(1)); 1; 0];
I  = find(e(5,:) == 3); % Randsegment 3
RDW_3(:,1) = [e(1,I(1)); 1; 0];
I  = find(e(5,:) == 4); % Randsegment 4
RDW_4(:,1) = [e(1,I(1)); 1; 0];

RDW = [RDW_1, RDW_2, RDW_3, RDW_4];

% -- DIRICHLET BOUNDARY CONDITIONS FOR TEMPERATUR
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDT_2 = [e(1,I); TEMPRECHTS*ones(1,LI)];
I  = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDT_4 = [e(1,I); TEMPLINKS*ones(1,LI)];
RDT = [RDT_2, RDT_4];
RCT = [];
