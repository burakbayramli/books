function [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = bsp07h(p,e,p1)
% Flow past half cylinder, boundary data
% RDU = [indices of boundary points, indices of midpoints in [p,p1];
%        boundary values of U at these points] 
% Seg.-nr. of ordered boundary: 1,2,3,7,8,9,5,6
% Order of Segm. arbitrary but here counterclockwise

M = size(p,2);
% -- BOUNDARY CONDITIONS for U and V
I = find(e(5,:) == 1); LI = length(I); % seg.1 
AUX = [e(1,I); zeros(1,LI)];
RDU1 = [AUX, [e(8,I); zeros(1,LI)]]; RDV1 = RDU1;

I = find(e(5,:) == 2); LI = length(I); % seg.2
AUX = [e(1,I); zeros(1,LI)];
RDU2 = [AUX, [e(8,I); zeros(1,LI)]]; RDV2 = RDU2;

I = find(e(5,:) == 3); LI = length(I); % seg.3
AUX = [e(1,I); zeros(1,LI)];
RDU3 = [AUX, [e(8,I); zeros(1,LI)]]; RDV3 = RDU3;

I = find(e(5,:) == 7); LI = length(I); % seg. 7
AUX = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];  
RDU7 = [AUX, [e(8,I); zeros(1,LI)]]; RDV7 = RDU7;
 
% boundary segment 8: outflow: no condition for U
I = find(e(5,:) == 8); LI = length(I); % seg.8
AUX = [e(1,I(2:LI)); zeros(1,LI-1)];
RDV8 = [AUX, [e(8,I); zeros(1,LI)]];

I = find(e(5,:) == 9); LI = length(I); % seg.9
[U1,V1] = inflow(p(1,e(1,I)),p(2,e(1,I))); 
AUX = [e(1,I); U1];
[U2,V2] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M)); 
RDU9 = [AUX, [e(8,I); U2]];
AUX = [e(1,I); V1];
RDV9 = [AUX, [e(8,I); V2]];

I = find(e(5,:) == 5); LI = length(I); % seg.5
[U1,V1] = inflow(p(1,e(1,I)),p(2,e(1,I))); 
AUX = [e(1,I); U1];
[U2,V2] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M)); 
RDU5 = [AUX, [e(8,I); U2]];
AUX = [e(1,I); V1];
RDV5 = [AUX, [e(8,I); V2]];

I = find(e(5,:) == 6); LI = length(I); % seg.6  inflow
[U1,V1] = inflow(p(1,e(1,I)),p(2,e(1,I))); 
AUX = [e(1,I); U1];
[U2,V2] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M)); 
RDU6 = [AUX, [e(8,I); U2]];
AUX = [e(1,I); V1];
RDV6 = [AUX, [e(8,I); V2]];

RDU = [RDU1, RDU2, RDU3, RDU7,       RDU9, RDU5, RDU6];
RDV = [RDV1, RDV2, RDV3, RDV7, RDV8, RDV9, RDV5, RDV6];

% -- CONDITION for P (one value)
N1 = size(p,2);
RDP = [N1;0];

% -- LOADS
FU = zeros(size(p,2)+size(p1,2),1); FV = FU;

% -- DIRICHLET BOUNDARY CONDITIONS FOR STREAM FUNCTION
% -- only for post prozessor
I  = find(e(5,:) == 1); LI = length(I); % seg.1
RDZ1 = [e(1,I); zeros(1,LI)];
RDZ1 = [RDZ1,[e(8,I); zeros(1,LI)]]; % midpoints

I  = find(e(5,:) == 2); LI = length(I); % seg.2
RDZ2 = [e(1,I); zeros(1,LI)];
RDZ2 = [RDZ2,[e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 3); LI = length(I); % seg.3
RDZ3 = [e(1,I); zeros(1,LI)];
RDZ3 = [RDZ3,[e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 7); LI = length(I); % seg.7
RDZ7 = [e(1,I); zeros(1,LI)];
RDZ7 = [RDZ7,[e(8,I); zeros(1,LI)]];

I  = find(e(5,:) == 9); LI = length(I); % seg. 9
RDZ9 = [e(1,I); inflow_z(p(1,e(1,I)),p(2,e(1,I)))];
RDZ9 = [RDZ9,[e(8,I); inflow_z(p1(1,e(8,I)-M),p1(2,e(8,I)-M))]];

I  = find(e(5,:) == 5); LI = length(I); % segm.5
RDZ5 = [e(1,I); inflow_z(p(1,e(1,I)),p(2,e(1,I)))];
RDZ5 = [RDZ5,[e(8,I); inflow_z(p1(1,e(8,I)-M),p1(2,e(8,I)-M))]];

I  = find(e(5,:) == 6); LI = length(I); % seg.6 inflow
RDZ6 = [e(1,I); inflow_z(p(1,e(1,I)),p(2,e(1,I)))];
RDZ6 = [RDZ6,[e(8,I); inflow_z(p1(1,e(8,I)-M),p1(2,e(8,I)-M))]];

RDZ = [RDZ1,RDZ2,RDZ3,RDZ7,RDZ9,RDZ5,RDZ6];
RCZ = [];

function [U,V]= inflow(X,Y)
A = 0.1; LX = length(X); % constant
U = A*ones(1,LX); V = zeros(1,LX);

function Z = inflow_z(X,Y)
% Z = int_0^y u(y)dy
A = 0.1; % constant
Z = A*Y;  
