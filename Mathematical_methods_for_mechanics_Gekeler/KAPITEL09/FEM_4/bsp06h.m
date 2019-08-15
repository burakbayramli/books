function [RDZ,RCZ,RDW,RDT,RCT] = bsp06h(p,e,t,T_EXACT)
% convection in a square box, exact example
% for coupled stream-vorticity-convection equations

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

RDW = [];

% -- DIRICHLET BOUNDARY CONDITIONS FOR TEMPERATUR
I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDT_1 = [e(1,I);T_EXACT(e(1,I))'];
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDT_2 = [e(1,I);T_EXACT(e(1,I))'];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDT_3 = [e(1,I);T_EXACT(e(1,I))'];
I  = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDT_4 = [e(1,I);T_EXACT(e(1,I))'];

RDT = [RDT_1, RDT_2 RDT_3, RDT_4];
RCT = [];
