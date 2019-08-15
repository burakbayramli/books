function [RDUN,RDVN] = bsp01h_tst(p,e,p1)
% lid driven cavity,
% boundary conditions for Taylor-Hood-Element
% RDU = [left terminal point of edge;
%       data;
%       data of midpoints]
% with improvements by R.Vanselow

% -- BOUNDARY CONDITIONS for U and V
I = find(e(5,:) == 1); LI = length(I);    % boundary segment 1 (below)
RDU1 = [e(1,I(2:LI)); zeros(1,LI-1)];     % left open, right open
RDU1 = [RDU1, [e(8,I); zeros(1,LI)]];
RDV1 = RDU1;

flag = 1;
switch flag
case 1
   I = find(e(5,:) == 2); LI = length(I);     % boundary segment 2 (right)
   RDU2 = [e(1,I); zeros(1,LI)];              % below closed, above open 
   RDU2 = [RDU2, [e(8,I); zeros(1,LI)]];
   RDV2 = RDU2;
case 2
   I = find(e(5,:) == 2); LI = length(I);     % boundary segment 2 (right)
   RDU2 = [e(1,I(2:LI)); zeros(1,LI-1)];      % open,open
   RDU2 = [RDU2, [e(8,I); zeros(1,LI)]];
   RDV2 = RDU2;
end
I = find(e(5,:) == 3); LI = length(I);     % boundary segment 3 (above)
RDU3 = [e(1,I(2:LI)); ones(1,LI-1)];       % U right open, left open 
RDU3 = [RDU3, [e(8,I); ones(1,LI)]];

I = find(e(5,:) == 3); LI = length(I);     % boundary segment 3 (above)
RDV3 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];  % V closed, closed
RDV3 = [RDV3, [e(8,I); zeros(1,LI)]];

I = find(e(5,:) == 4); LI = length(I);     % boundary segment 4 (left)
RDU4 = [e(1,I),e(2,I(LI)); zeros(1,LI+1)]; % above closed, below closed
RDU4 = [RDU4, [e(8,I); zeros(1,LI)]];
RDV4 = RDU4;  

RDUN = [RDU2, RDU4];
RDVN = [RDV1, RDV3];


