function [RDU,RDV,RDP,FU,FV,RDZ,RCZ,RCP] = bsp05h(p,e,p1,b)
% Example: Example after Boukir with exact solution
% boundary conditions for Taylor-Hood-Element
% RDU = [indices of boundary points, indices of midpoints in [p,p1];
%        boundary values of U at these points] 

M = size(p,2);

I  = find(e(5,:) == 1); LI = length(I); % boundary segment 1 (below)
[U1,V1,Z1] = fcn1(p(1,e(1,I)),p(2,e(1,I)),b);
RDU1 = [e(1,I);U1]; RDV1 = [e(1,I);V1]; RDZ1 = [e(1,I);Z1]; 
[U1,V1,Z1] = fcn1(p1(1,e(8,I)-M),p1(2,e(8,I)-M),b);
RDU1 = [RDU1,[e(8,I); U1]]; RDV1 = [RDV1,[e(8,I); V1]];
RDZ1 = [RDZ1,[e(8,I);Z1]];

I  = find(e(5,:) == 2); LI = length(I); % boundary segment 2 (right)
[U1,V1,Z1] = fcn2(p(1,e(1,I)),p(2,e(1,I)),b);
RDU2 = [e(1,I);U1]; RDV2 = [e(1,I);V1]; RDZ2 = [e(1,I);Z1]; 
[U1,V1,Z1] = fcn2(p1(1,e(8,I)-M),p1(2,e(8,I)-M),b);
RDU2 = [RDU2,[e(8,I); U1]]; RDV2 = [RDV2,[e(8,I); V1]];
RDZ2 = [RDZ2,[e(8,I);Z1]] ;

I  = find(e(5,:) == 3); LI = length(I); % boundary segment 3 (above)
[U1,V1,Z1] = fcn3(p(1,e(1,I)),p(2,e(1,I)),b);
RDU3 = [e(1,I);U1]; RDV3 = [e(1,I);V1]; RDZ3 = [e(1,I);Z1]; 
[U1,V1,Z1] = fcn3(p1(1,e(8,I)-M),p1(2,e(8,I)-M),b);
RDU3 = [RDU3,[e(8,I); U1]]; RDV3 = [RDV3,[e(8,I); V1]];
RDZ2 = [RDZ2,[e(8,I);Z1]] ;

I  = find(e(5,:) == 4); LI = length(I); % boundary segment 4 (left)
[U1,V1,Z1] = fcn4(p(1,e(1,I)),p(2,e(1,I)),b);
RDU4 = [e(1,I);U1]; RDV4 = [e(1,I);V1]; RDZ4 = [e(1,I);Z1]; 
[U1,V1,Z1] = fcn4(p1(1,e(8,I)-M),p1(2,e(8,I)-M),b);
RDU4 = [RDU4,[e(8,I); U1]]; RDV4 = [RDV4,[e(8,I); V1]];
RDZ4 = [RDZ4,[e(8,I);Z1]] ;

RDU = [RDU1, RDU2, RDU3, RDU4];
RDV = [RDV1, RDV2, RDV3, RDV4];
RDZ = [RDZ1, RDZ2, RDZ3, RDZ4];

% -- CONDITION for P (one value)
RDP = [9;0];  % optimal choice, other choices bad for pressure 

%J  = size(p,2); % bad result in DAE problems, (good in stat. problems)
%X = p(1,J); Y = p(2,J);
%RDP = [J;2*pi*cos(pi*X)*sin(pi*Y)];

% -- LOADS ----------------------------
X = [p(1,:),p1(1,:)]; Y = [p(2,:),p1(2,:)];
FU = pi*b^2*cos(pi*X).*sin(pi*X);
FV = -pi*b^2*sin(pi*Y).*cos(pi*Y) + 4*pi^2*cos(pi*X).*cos(pi*Y);
FU = FU.';
FV = FV.';

RCZ  = [];

% -- data for pressure phase ---------------------
RCP  = [];

function [U,V,Z]= fcn1(X,Y,b); % Seg. 1, below 
   LX = length(X);
   U = -b*sin(pi*X);
   V = zeros(1,LX);
   Z = [];
   % Z = int_0^y u(y)dy
function [U,V,Z]= fcn2(X,Y,b); % Seg. 2, right
   LX = length(X);
   U = zeros(1,LX);
   V = -b*cos(pi*Y);
   Z = []; % TODO
   % Z = int_0^y u(y)dy

function [U,V,Z]= fcn3(X,Y,b); % Seg. 3, above
   LX = length(X);
   U = b*sin(pi*X);
   V = zeros(1,LX);
   Z = []; % TODO
   % Z = int_0^y u(y)dy

function [U,V,Z]= fcn4(X,Y,b); % Seg. 4, left
   LX = length(X);
   U =  zeros(1,LX);
   V = b*cos(pi*Y);
   Z = []; % TODO
   % Z = int_0^y u(y)dy

