function [RDZ,RCZ,RDW] = bsp08h(p,e,t,Parmeter)
% unit square, exact example, elliptic system
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- CAUCHY BOUNDARY CONDITIONS FOR STREAM FUNCTION
% z_n = 0 auf dem ganzen Rand
RCZ = [];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- DIRICHLET BOUNDARY CONDITIONS FOR VORTICITY
RDW = [];

