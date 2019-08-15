function [RDZ,RCZ,RDW] = bsp06h2(e,Parmeter,W0)
% test example with exact solution, time dependent
% Exact BOUNDARY FOR W
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
RCZ = [];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- BOUNDARY CONDITIONS FOR VORTICITY
I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDW_1 = [e(1,I); ones(1,LI); W0(e(1,I)).'];

I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDW_2 = [e(1,I); ones(1,LI); W0(e(1,I)).'];

I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDW_3 = [e(1,I); ones(1,LI); W0(e(1,I)).'];

I  = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDW_4 = [e(1,I); ones(1,LI); W0(e(1,I)).'];

RDW = [RDW_1, RDW_2, RDW_3, RDW_4];
