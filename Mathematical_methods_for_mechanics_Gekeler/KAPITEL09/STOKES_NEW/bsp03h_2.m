function [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = bsp03h_2(p,e,p1,U0)
% Flow past half cylinder, boundary data
% RDU = [indices of boundary points, indices of midpoints in [p,p1];
%        boundary values of U at these points] 
% Seg.-nr. of ordered boundary: 1,2,3,7,8,9,5,6
% Order of Segm. arbitrary but here counterclockwise

M = size(p,2);
% -- BOUNDARY CONDITIONS for U and V
I = find(e(5,:) == 1); LI = length(I); % seg.1 
RDU1 = [e(1,I),e(8,I); zeros(1,2*LI)]; RDV1 = RDU1;

I = find(e(5,:) == 2); LI = length(I); % seg.2
RDU2 = [e(1,I),e(8,I); zeros(1,2*LI)]; RDV2 = RDU2;

I = find(e(5,:) == 3); LI = length(I); % seg.3
RDU3 = [e(1,I),e(8,I); zeros(1,2*LI)]; RDV3 = RDU3;

I = find(e(5,:) == 7); LI = length(I); % seg. 7
RDU7 = [e(1,I),e(2,I(LI)),e(8,I); zeros(1,2*LI+1)]; RDV7 = RDU7;
 
% boundary segment 8: outflow: no condition for U
I = find(e(5,:) == 8); LI = length(I); % seg.8
RDV8 = [e(1,I(2:LI)),e(8,I); zeros(1,2*LI-1)];

I = find(e(5,:) == 9); LI = length(I); % seg.9
[U1,V1] = inflow(p(1,e(1,I)),p(2,e(1,I)),U0); 
AUX = [e(1,I); U1];
[U2,V2] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M),U0); 
RDU9 = [AUX, [e(8,I); U2]];
RDV9 = [e(1,I),e(8,I); V1,V2];

I = find(e(5,:) == 5); LI = length(I); % seg.5
[U1,V1] = inflow(p(1,e(1,I)),p(2,e(1,I)),U0); 
AUX = [e(1,I); U1];
[U2,V2] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M),U0); 
RDU5 = [AUX, [e(8,I); U2]];
RDV5 = [e(1,I),e(8,I); V1,V2];

I = find(e(5,:) == 6); LI = length(I); % seg.6  inflow
[U1,V1] = inflow(p(1,e(1,I)),p(2,e(1,I)),U0); 
AUX = [e(1,I); U1];
[U2,V2] = inflow(p1(1,e(8,I)-M),p1(2,e(8,I)-M),U0); 
RDU6 = [AUX, [e(8,I); U2]];
RDV6 = [e(1,I),e(8,I); V1,V2];

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
RDZ1 = [e(1,I),e(8,I); zeros(1,2*LI)];

I  = find(e(5,:) == 2); LI = length(I); % seg.2
RDZ2 = [e(1,I),e(8,I); zeros(1,2*LI)];

I  = find(e(5,:) == 3); LI = length(I); % seg.3
RDZ3 = [e(1,I),e(8,I); zeros(1,2*LI)];

I  = find(e(5,:) == 7); LI = length(I); % seg.7
RDZ7 = [e(1,I),e(8,I); zeros(1,2*LI)];

I  = find(e(5,:) == 9); LI = length(I); % seg. 9
RDZ9 = [e(1,I); inflow_z(p(1,e(1,I)),p(2,e(1,I)),U0)];
RDZ9 = [RDZ9,[e(8,I); inflow_z(p1(1,e(8,I)-M),p1(2,e(8,I)-M),U0)]];

I  = find(e(5,:) == 5); LI = length(I); % segm.5
RDZ5 = [e(1,I); inflow_z(p(1,e(1,I)),p(2,e(1,I)),U0)];
RDZ5 = [RDZ5,[e(8,I); inflow_z(p1(1,e(8,I)-M),p1(2,e(8,I)-M),U0)]];

I  = find(e(5,:) == 6); LI = length(I); % seg.6 inflow
RDZ6 = [e(1,I); inflow_z(p(1,e(1,I)),p(2,e(1,I)),U0)];
RDZ6 = [RDZ6,[e(8,I); inflow_z(p1(1,e(8,I)-M),p1(2,e(8,I)-M),U0)]];

RDZ = [RDZ1,RDZ2,RDZ3,RDZ7,RDZ9,RDZ5,RDZ6];
RCZ = [];

function [U,V]= inflow(X,Y,U0)
LX = length(X); % constant
U = U0*ones(1,LX); V = zeros(1,LX);

function Z = inflow_z(X,Y,U0)
% Z = int_0^y u(y)dy
Z = U0*Y;  
