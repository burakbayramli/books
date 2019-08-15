function [RDZ,RCZ,RDW,RDT,RCT] = bsp05h(p,e,t)
% convection in a unit square 
% for coupled stream-vorticity-convection equations
TEMPLINKS = 0;
TEMPRECHTS = 1;

% -- DIRICHLET BOUNDARY CONDITIONS FOR STREAM FUNCTION
I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDZ_1 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDZ_2 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDZ_3 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDZ_4 = [e(1,I); zeros(1,LI)];
RDZ = [RDZ_1, RDZ_2, RDZ_3, RDZ_4];

% -- CAUCHY BOUNDARY CONDITIONS FOR STREAM FUNCTION
RCZ = []; % sonst mind. quadratische Elemente nehmen!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- DIRICHLET BOUNDARY CONDITIONS FOR VORTICITY
RDW = [];

% -- DIRICHLET BOUNDARY CONDITIONS FOR TEMPERATUR
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2, [D]
RDT_2 = [e(1,I),e(2,I(LI)); TEMPRECHTS*ones(1,LI+1)];
I  = find(e(5,:) == 4); LI = length(I); % Randsegment 4, [B]
RDT_4 = [e(1,I),e(2,I(LI)); TEMPLINKS*ones(1,LI+1)];
RDT = [RDT_2, RDT_4];
RCT = [];
