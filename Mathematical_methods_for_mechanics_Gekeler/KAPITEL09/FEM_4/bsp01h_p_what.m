function [RDU,RDV,RDP] = bsp01h_p(p,e)

% lid driven cavity,
% boundary conditions for Taylor-Hood-Element
% RDU = [left terminal point of edge;
%       data;
%       data of midpoints]
% -- BOUNDARY CONDITIONS for U
I = find(e(5,:) == 1); LI = length(I); % boundary segment 1
RDU1 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
%RDU1 = [RDU1, [e(6,I); zeros(1,LI)]];

I = find(e(5,:) == 2); LI = length(I); % boundary segment 2
RDU2 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
%RDU2 = [RDU2, [e(6,I); zeros(1,LI)]];

I = find(e(5,:) == 3); LI = length(I); % boundary segment 3
RDU3 = [e(1,I(2:LI)); ones(1,LI-1)];
%RDU3 = [RDU3, [e(6,I); ones(1,LI)]];

I = find(e(5,:) == 4); LI = length(I); % boundary segment 4
RDU4 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
%RDU4 = [RDU4, [e(6,I); zeros(1,LI)]];

RDU = [RDU2, RDU4];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- BOUNDARY CONDITIONS for V
I = find(e(5,:) == 1); LI = length(I); % boundary segment 1
RDV1 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
%RDV1 = [RDV1, [e(6,I); zeros(1,LI)]];

I = find(e(5,:) == 2); LI = length(I); % boundary segment 2
RDV2 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
%RDV2 = [RDV2, [e(6,I); zeros(1,LI)]];

I = find(e(5,:) == 3); LI = length(I); % boundary segment 3
RDV3 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
%RDV3 = [RDV3, [e(6,I); zeros(1,LI)]];

I = find(e(5,:) == 4); LI = length(I); % boundary segment 4
RDV4 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
%RDV4 = [RDV4, [e(6,I); zeros(1,LI)]];

RDV = [RDV1, RDV3];
I = find(e(5,:) == 2); LI = length(I); % Randsegment 1
RDP = [e(1,I(1)); 1E5];


