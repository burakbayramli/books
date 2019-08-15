function [RDU,RDV,RDP,FU,FV] = bsp08h(p,e,p1)
% Letters F E M
% RDU = [indices of boundary points, indices of midpoints in [p,p1];
%        boundary values of U at these points] 

% -- BOUNDARY CONDITIONS for U and V
I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDU1 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
RDU1 = [RDU1, [e(8,I); zeros(1,LI)]];
RDV1 = [e(1,I),e(2,I(LI)); ones(1,LI+1)];
RDV1 = [RDV1, [e(8,I); ones(1,LI)]];

I = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDU2 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
RDU2 = [RDU2, [e(8,I); zeros(1,LI)]];      RDV2 = RDU2;

I = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDU3 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
RDU3 = [RDU3, [e(8,I); zeros(1,LI)]];
RDV3 = [e(1,I),e(2,I(LI)); -ones(1,LI+1)];
RDV3 = [RDV3, [e(8,I); -ones(1,LI)]];


I = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDU4 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
RDU4 = [RDU4, [e(8,I); zeros(1,LI)]];     RDV4 = RDU4;

RDU = [RDU1, RDU2, RDU3, RDU4];
RDV = [RDV1, RDV2, RDV3, RDV4];

% -- CONDITION for P (one value)
N1 = size(p,2);
RDP = [N1;0];

% -- Loads ---------------------------------
FU = zeros(size(p,2)+size(p1,2),1); FV = FU;
