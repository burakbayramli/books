function [RDZ,RCZ,RDW] = bsp07h(p,e,t,Parmeter)
VS = Parmeter(3);
% lid driven cavity, elliptic system
% -- DIRICHLET BOUNDARY CONDITIONS FOR STREAM FUNCTION
I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDZ_1 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDZ_2 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDZ_3 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDZ_4 = [e(1,I); zeros(1,LI)];
RDZ = [RDZ_1, RDZ_2, RDZ_3, RDZ_4];
RDZ = RDZ_3;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- CAUCHY BOUNDARY CONDITIONS FOR STREAM FUNCTION
% slip boundary condition with VS
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RCZ_3 = [e(1,I); e(2,I); VS*ones(1,LI)];
RCZ = RCZ_3;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- DIRICHLET BOUNDARY CONDITIONS FOR W
I  = find(e(5,:) == 1); % Randsegment 1
RDW_1(:,1) = [e(1,I(1)); 0];
I  = find(e(5,:) == 2); % Randsegment 2
RDW_2(:,1) = [e(1,I(1)); 0];
I  = find(e(5,:) == 3); % Randsegment 3
RDW_3(:,1) = [e(1,I(1)); 0];
I  = find(e(5,:) == 4); % Randsegment 4
RDW_4(:,1) = [e(1,I(1)); 0];
RDW = [RDW_1, RDW_2, RDW_3, RDW_4];
