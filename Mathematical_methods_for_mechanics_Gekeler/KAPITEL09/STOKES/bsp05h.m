function [RDU,RDV,RDP,FU,FV] = bsp05h(p,e,p1)
% unit square, example with exact solution:
% boundary conditions for Taylor-Hood-Element
% RDU = [indices of boundary points, indices of midpoints in [p,p1];
%        boundary values of U at these points] 
% Flow    : (U,V) = (x^3,-3*x^2*y)
% Pressure: P     = x^3 + y^3

% Exact values:
N1 = size(p,2); N2 = size(p1,2);
X = p(1,:); Y = p(2,:); X1 = p1(1,:); Y1 = p1(2,:);
UE  = X.^3;  VE  = - 3*X.*X.*Y;
UE1 = X1.^3; VE1 = - 3*X1.*X1.*Y1;
PE = X.^3 + Y.^3; PE1 = X1.^3 + Y1.^3;

% -- BOUNDARY CONDITIONS for U and V
I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDU1 = [e(1,I); UE(e(1,I))];
RDU1 = [RDU1, [e(8,I); UE1(e(8,I)-N1)]];
RDV1 = [e(1,I); VE(e(1,I))];
RDV1 = [RDV1, [e(8,I); VE1(e(8,I) - N1)]];

I = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDU2 = [e(1,I); UE(e(1,I))];
RDU2 = [RDU2, [e(8,I); UE1(e(8,I)-N1)]];
RDV2 = [e(1,I); VE(e(1,I))];
RDV2 = [RDV2, [e(8,I); VE1(e(8,I)-N1)]];

I = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDU3 = [e(1,I); UE(e(1,I))];
RDU3 = [RDU3, [e(8,I); UE1(e(8,I)-N1)]];
RDV3 = [e(1,I); VE(e(1,I))];
RDV3 = [RDV3, [e(8,I); VE1(e(8,I)-N1)]];

I = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDU4 = [e(1,I); UE(e(1,I))];
RDU4 = [RDU4, [e(8,I); UE1(e(8,I)-N1)]];
RDV4 = [e(1,I); VE(e(1,I))];
RDV4 = [RDV4, [e(8,I); VE1(e(8,I)-N1)]];

RDU = [RDU1, RDU2, RDU3, RDU4];
%RDU = [RDU2,RDU4];
RDV = [RDV1, RDV2, RDV3, RDV4];
%RDV = [RDV1, RDV2, RDV3];  % vgl. RDU

% DU/DN = 0 on boundary 1 and 3 exact and in limit,
% same result both times

% -- CONDITION for P (one value)
RDP = [3;PE(3)];,
RDP = [1;PE(1)];

% -- LOADS ------------------------
p3 = [p,p1];
FU = (-6*p3(1,:) + 3*p3(1,:).^2).';
FV = ( 6*p3(2,:) + 3*p3(2,:).^2).';
