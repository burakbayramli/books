function [RDU,RDV,RDZ] = bsp03h(p,e,V,T,Parmeter);
% Boundary data for channel
A = Parmeter(1); PERIOD = Parmeter(2); g = Parmeter(3);
omga = 2*pi/PERIOD;
% -- U-Teil -------------------------
I = find(e(5,:) == 2); LI = length(I); % Rand 2
E = [e(1,I), e(2,I(LI))];
AUX = p(3,E) + V(3,E);
% open boundary
RDU_2 = [E;sqrt(g)*V(3,E)./sqrt(AUX)];
% closed boundary
%RDU_2 = [E;zeros(1,LI+1)];
RDU = RDU_2;

% -- V-Teil --------------------
I = find(e(5,:) == 1); LI = length(I); % Rand 1
RDV_1 = [e(1,I);zeros(1,LI)];
I = find(e(5,:) == 2); LI = length(I); % Rand 2
RDV_2 = [e(1,I);zeros(1,LI)];
I = find(e(5,:) == 3); LI = length(I); % Rand 3
RDV_3 = [e(1,I);zeros(1,LI)];
I = find(e(5,:) == 4); LI = length(I); % Rand 4
RDV_4 = [e(1,I);zeros(1,LI)];
RDV = [RDV_1,RDV_2,RDV_3,RDV_4];

% -- Z-Teil ----------------------
I = find(e(5,:) == 4); LI = length(I); % Rand 4
RDZ_4 = [e(1,I),e(2,I(LI));A*sin(omga*T)*ones(1,LI+1)];
RDZ = RDZ_4;
