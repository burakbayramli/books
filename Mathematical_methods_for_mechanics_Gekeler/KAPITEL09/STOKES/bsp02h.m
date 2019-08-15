function [RDU,RDV,RDP] = bsp02h(p,e)
% lid driven cavity,
% boundary conditions for Mini-element
% RDU = [indices of boundary points;
%        boundary values of U  at these points] 

% -- BOUNDARY CONDITIONS for U
I = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDU2 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];

I = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDU3 = [e(1,I(2:LI)); ones(1,LI-1)];

I = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDU4 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];

RDU = [RDU2, RDU3, RDU4];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- BOUNDARY CONDITIONS for V
I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDV1 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];

I = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDV3 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];

RDV = [RDV1, RDV3];

% -- CONDITION for P (one value)
N1 = size(p,2);
RDP = [N1;0];

