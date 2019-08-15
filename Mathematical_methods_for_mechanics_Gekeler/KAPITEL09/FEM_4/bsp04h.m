function [RDZ,RCZ,RDW,RDT,RCT] = bsp04h(p,e,t)
% thermal flow in a cup
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- DIRICHLET BOUNDARY CONDITIONS FOR VORTICITY
% in Ecken W = 0 setzen

I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDW_3 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];

RDW = RDW_3;
% -- DIRICHLET BOUNDARY CONDITIONS FOR TEMPERATUR
I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDT_1 = [e(1,I),e(2,I(LI)); 60*ones(1,LI+1)];
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDT_2 = [e(1,I(2:LI)); 15*ones(1,LI-1)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDT_3 = [e(1,I(2:LI)); 15*ones(1,LI-1)];
I  = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDT_4 = [e(1,I(1:LI)); 15*ones(1,LI)];

RDT = [RDT_1, RDT_2, RDT_4];
RDT = [RDT_1, RDT_3];

% -- CAUCHY BOUNDARY FOR TEMPERATUR
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RCT = e(:,I); % Werte siehe RIGHTSIDES.M
