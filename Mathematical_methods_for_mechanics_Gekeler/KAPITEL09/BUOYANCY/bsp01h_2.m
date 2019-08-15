function [RDU,RDV,RDT,RDP,FU,FV,FT,RDZ,RCZ,RCT] = bsp01h_2(p,e,p1,parmeter)
% thermal flow in a cup
% boundary conditions for Taylor-Hood-Element
% same as bsp01h_1.m but Cauchy boundary conditions on surface
% RDU = [indices of boundary points, indices of midpoints in [p,p1];
%        boundary values of U at these points] 

if nargin == 3, parmeter = []; end

% -- BOUNDARY CONDITIONS for U and V
I = find(e(5,:) == 1); LI = length(I);   % boundary segment 1 (below)
RDU1 = [e(1,I),e(8,I); zeros(1,2*LI)];   % left closed, right open
RDV1 = RDU1;

I = find(e(5,:) == 2); LI = length(I);   % boundary segment 2 (right)
RDU2 = [e(1,I),e(8,I); zeros(1,2*LI)];   % below closed, above open 
RDV2 = RDU2;

I = find(e(5,:) == 3); LI = length(I);   % boundary segment 3 (above)
RDV3 = [e(1,I),e(8,I); zeros(1,2*LI)];   % right closed, left open 

I = find(e(5,:) == 5); LI = length(I);   % boundary segment 5 (above)
RDV5 = [e(1,I),e(2,I(LI)),e(8,I);zeros(1,2*LI+1)]; % right closed, left closed 

I = find(e(5,:) == 6); LI = length(I);   % boundary segment 6 (left)
RDU6 = [e(1,I(2:LI)),e(2,I(LI)),e(8,I); zeros(1,2*LI)]; % above open, below closed
RDV6 = RDU6;  

I = find(e(5,:) == 7); LI = length(I);   % boundary segment 7 (below left)
RDU7 = [e(1,I(2:LI)),e(8,I); zeros(1,2*LI-1)]; % left open, right open
RDV7 = RDU7;

RDU = [RDU1, RDU2, RDU6, RDU7];
RDV = [RDV1, RDV2, RDV3, RDV5, RDV6, RDV7];

% -- Data for P  ------
%N1 = size(p,2);
RDP = [31;0]; % zero at midpoint of upper boundary

% -- DIRICHLET BOUNDARY CONDITIONS FOR TEMPERATUR
I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDT1 = [e(1,I),e(8,I); 60*ones(1,2*LI)];           %closed/ open

I  = find(e(5,:) == 7); LI = length(I); % Randsegment 7
RDT7 = [e(1,I(2:LI)),e(8,I); 60*ones(1,2*LI-1)]; % open/open 

I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDT2 = [e(1,I),e(8,I); 15*ones(1,2*LI)]; % closed/open

I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDT3 = [e(1,I),e(8,I); 15*ones(1,2*LI)]; %closed/open

I  = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDT5 = [e(1,I),e(2,I(LI)),e(8,I); 15*ones(1,2*LI+1)]; %closed/closed

I  = find(e(5,:) == 6); LI = length(I); % Randsegment 6
RDT6 = [e(1,I(2:LI)),e(2,I(LI)),e(8,I); 15*ones(1,2*LI)]; %open/closed


RDT = [RDT1, RDT2, RDT6,RDT7];
%RDT = RDT1;

% -- Loads
N = size(p,2) + size(p1,2);
FU = zeros(N,1); FV = FU; FT = FU;

% -- CAUCHY BOUNDARY FOR TEMPERATUR
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RCT = e([1,8,2],I);
I  = find(e(5,:) == 5); LI = length(I); % Randsegment 3
RCT = [RCT,e([1,8,2],I)];

% -- BOUNDARY CONDITIONS for stream function Z (for postprozessing)
I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDZ1 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDZ2 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDZ3 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDZ5 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 6); LI = length(I); % Randsegment 6
RDZ6 = [e(1,I),e(8,I); zeros(1,2*LI)];
I  = find(e(5,:) == 7); LI = length(I); % Randsegment 7
RDZ7 = [e(1,I),e(8,I); zeros(1,2*LI)];

RDZ = [RDZ1, RDZ2, RDZ3, RDZ5, RDZ6, RDZ7];
RCZ = [];


  