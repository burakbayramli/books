function [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = bsp10h(p,e,p1)
% Flow past cylinder, boundary data
% RDU = [indices of boundary points, indices of midpoints in [p,p1];
%        boundary values of U at these points] 
% segment nrs. for outer boundary = [1,7,8,9,3,4]; 
% segment nrs. for inner boundary = [5,6]; 

M = size(p,2); U0 = 4; % regard subfunction !!
% -- BOUNDARY CONDITIONS for U and V
I = find(e(5,:) == 1); LI = length(I); % seg.1 
AUX = [e(1,I); U0*ones(1,LI)];
RDU1 = [AUX, [e(8,I); U0*ones(1,LI)]]; 
AUX = [e(1,I); zeros(1,LI)];
RDV1 = [AUX, [e(8,I); zeros(1,LI)]]; 

I = find(e(5,:) == 7); LI = length(I); % seg.7 
AUX = [e(1,I); U0*ones(1,LI)];
RDU7 = [AUX, [e(8,I); U0*ones(1,LI)]]; 
AUX = [e(1,I); zeros(1,LI)];
RDV7 = [AUX, [e(8,I); zeros(1,LI)]]; 

I = find(e(5,:) == 8); LI = length(I); % seg.8
AUX = [e(1,I); zeros(1,LI)];
RDV8 = [AUX, [e(8,I); zeros(1,LI)]]; 

I = find(e(5,:) == 9); LI = length(I); % seg.9 
AUX = [e(1,I); U0*ones(1,LI)];
RDU9 = [AUX, [e(8,I); U0*ones(1,LI)]]; 
AUX = [e(1,I); zeros(1,LI)];
RDV9 = [AUX, [e(8,I); zeros(1,LI)]]; 

I = find(e(5,:) == 3); LI = length(I); % seg.3 
AUX = [e(1,I); U0*ones(1,LI)];
RDU3 = [AUX, [e(8,I); U0*ones(1,LI)]]; 
AUX = [e(1,I); zeros(1,LI)];
RDV3 = [AUX, [e(8,I); zeros(1,LI)]]; 

I = find(e(5,:) == 4); LI = length(I); % seg.4 
AUX = [e(1,I); U0*ones(1,LI)];
RDU4 = [AUX, [e(8,I); U0*ones(1,LI)]]; 
AUX = [e(1,I); zeros(1,LI)];
RDV4 = [AUX, [e(8,I); zeros(1,LI)]]; 

I = find(e(5,:) == 5); LI = length(I); % seg.5 
AUX = [e(1,I); zeros(1,LI)];
RDU5 = [AUX, [e(8,I); zeros(1,LI)]]; RDV5 = RDU5;

I = find(e(5,:) == 6); LI = length(I); % seg.6 
AUX = [e(1,I); zeros(1,LI)];
RDU6 = [AUX, [e(8,I); zeros(1,LI)]]; RDV6 = RDU6;

RDU = [RDU1,RDU7,     RDU9,RDU3,RDU4,RDU5,RDU6];
RDV = [RDV1,RDV7,RDV8,RDV9,RDV3,RDV4,RDV5,RDV6];

% -- CONDITION for P (one value)
N1 = size(p,2);
RDP = [N1;0];

% -- LOADS
FU = zeros(size(p,2)+size(p1,2),1); FV = FU;

% -- DIRICHLET BOUNDARY CONDITIONS FOR STREAM FUNCTION
% -- only for post prozessor
I = find(e(5,:) == 1); LI = length(I); % seg.1
[U,V,Z] = inflow(p(1,e(1,I)),p(2,e(1,I)));
AUX = [e(1,I); Z];
[U,V,Z] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M));
RDZ1 = [AUX,[e(8,I); Z]];

I = find(e(5,:) == 7); LI = length(I); % seg.7
[U,V,Z] = inflow(p(1,e(1,I)),p(2,e(1,I)));
AUX = [e(1,I); Z];
[U,V,Z] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M));
RDZ7 = [AUX,[e(8,I); Z]];

I = find(e(5,:) == 9); LI = length(I); % seg.9
[U,V,Z] = inflow(p(1,e(1,I)),p(2,e(1,I)));
AUX = [e(1,I); Z];
[U,V,Z] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M));
RDZ9 = [AUX,[e(8,I); Z]];

I = find(e(5,:) == 3); LI = length(I); % seg.3
[U,V,Z] = inflow(p(1,e(1,I)),p(2,e(1,I)));
AUX = [e(1,I); Z];
[U,V,Z] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M));
RDZ3 = [AUX,[e(8,I); Z]];

I = find(e(5,:) == 4); LI = length(I); % seg.4
[U,V,Z] = inflow(p(1,e(1,I)),p(2,e(1,I)));
AUX = [e(1,I); Z];
[U,V,Z] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M));
RDZ4 = [AUX,[e(8,I); Z]];

I  = find(e(5,:) == 5); LI = length(I); % seg.5
AUX = [e(1,I); zeros(1,LI)];
RDZ5 = [AUX,[e(8,I); zeros(1,LI)]]; % midpoints

I  = find(e(5,:) == 6); LI = length(I); % seg.6
AUX = [e(1,I); zeros(1,LI)];
RDZ6 = [AUX,[e(8,I); zeros(1,LI)]]; % midpoints

RDZ = [RDZ1,RDZ7,RDZ9,RDZ3,RDZ4,RDZ5,RDZ6];
%RDZ = [RDZ1,RDZ7,RDZ9,RDZ3,RDZ4];
RCZ = [];
% inflow for (a) with U0 = 4, (b) with U0 = 1
%function [U,V,Z]= inflow(X,Y);
%   U0 = 4; LX = length(X);
%   U = U0*ones(1,LX); V = zeros(1,LX);
%   % Z = int_0^y u(y)dy
%   Z = -20 + U0*Y; % symmetrically  !!
   
function [U,V,Z]= inflow(X,Y);
   U0 = 1; LX = length(X);
   U = U0*ones(1,LX); V = zeros(1,LX);
   % Z = int_0^y u(y)dy
   Z = -5 + U0*Y; % symmetrically  !!
