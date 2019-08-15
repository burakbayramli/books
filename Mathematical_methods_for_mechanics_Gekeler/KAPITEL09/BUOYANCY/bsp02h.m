function [RDU,RDV,RDT,RDP,FU,FV,FT,RDZ,RCZ] = bsp02h(p,e,p1,parmeter)
% convection in unit square
% boundary conditions for Taylor-Hood-Element
% RDU = [indices of boundary points, indices of midpoints in [p,p1];
%        boundary values of U at these points] 

if nargin == 3, parmeter = []; end
templeft = parmeter(1); tempright = parmeter(2);

% -- BOUNDARY CONDITIONS for U and V
I = find(e(5,:) == 1); LI = length(I);  % boundary segment 1 (below)
RDU1 = [e(1,I),e(8,I); zeros(1,2*LI)];  % closed/open
RDV1 = RDU1;

I = find(e(5,:) == 2); LI = length(I);  % boundary segment 2 (right)
RDU2 = [e(1,I),e(8,I); zeros(1,2*LI)];  % closed/open 
RDV2 = RDU2;

I = find(e(5,:) == 3); LI = length(I);  % boundary segment 3 (above)
RDU3 = [e(1,I),e(8,I); zeros(1,2*LI)];  % closed/open 
RDV3 = RDU3;

I = find(e(5,:) == 4); LI = length(I);  % boundary segment 4 (left)
RDU4 = [e(1,I),e(8,I); zeros(1,2*LI)];  % closed/open
RDV4 = RDU4;  

RDU = [RDU1, RDU2, RDU3, RDU4];
RDV = [RDV1, RDV2, RDV3, RDV4];

% -- Data for P (one value) ------
%N1 = size(p,2);
RDP = [1;0]; % zero at midpoint of bottom

% -- DIRICHLET BOUNDARY CONDITIONS FOR TEMPERATUR

I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2 (right)
RDT2 = [e(1,I),e(8,I); tempright*ones(1,2*LI)]; % closed/open 

I  = find(e(5,:) == 4); LI = length(I); % boundary segment 4 (left)
RDT4 = [e(1,I),e(8,I); templeft*ones(1,2*LI)]; %closed/open

RDT = [RDT2, RDT4];

% -- LOADS ----------------------------
FU = zeros(size(p,2)+size(p1,2),1); FV = FU; FT = FU;

% -- DIRICHLET BOUNDARY CONDITIONS FOR STREAM FUNCTION
I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDZ1 = [e(1,I); zeros(1,LI)];           % all closed/open
RDZ1 = [RDZ1,[e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDZ2 = [e(1,I); zeros(1,LI)];
RDZ2 = [RDZ2,[e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDZ3 = [e(1,I); zeros(1,LI)];
RDZ3 = [RDZ3,[e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDZ4 = [e(1,I); zeros(1,LI)];
RDZ4 = [RDZ4,[e(8,I); zeros(1,LI)]];

RDZ = [RDZ1,RDZ2,RDZ3,RDZ4];

RCZ = [];

