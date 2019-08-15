function [RDZ,RDW] = bsp05h(e,Parmeter)
% Navier for Transport
% -- BOUNDARY CONDITIONS FOR STREAM FUNCTION

VS = Parmeter(3);
I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDZ_1 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDZ_2 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDZ_3 = [e(1,I), e(2,I(LI)); zeros(1,LI+1)];
I  = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDZ_5 = [e(1,I), e(2,I(LI)); 2*34*ones(1,LI+1)];
I  = find(e(5,:) == 6); LI = length(I); % Randsegment 6
AUX = linspace(2*34,0,LI+1);
RDZ_6 = [e(1,I); AUX(1:LI)];

%%% RDZ = [RDZ_1,RDZ_2,RDZ_3,RDZ_5]; % fuer Beispiel
RDZ = [RDZ_1,RDZ_2,RDZ_3,RDZ_5,RDZ_6];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- BOUNDARY CONDITIONS FOR VORTICITY
% TYP 1: w = w_B; TYP 2: w = w_wall;
% TYP 3: w = w_b in segments with w = w_wall
% RD_W = [[Anf.-punkte]',TYP, Wert]

I  = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDW_1 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];
I  = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDW_2 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDW_3 = [e(1,I); 2*ones(1,LI); zeros(1,LI)];
I  = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDW_5 = [e(1,I); ones(1,LI); zeros(1,LI)];
I  = find(e(5,:) == 6); LI = length(I); % Randsegment 6
RDW_6 = [e(1,I); ones(1,LI); zeros(1,LI)];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Additional boundary conditions for W
% -- in segments with Wbound
I  = find(e(5,:) == 2); % Randsegment 2
RDW_2(:,1) = [e(1,I(1)); 1; 0];
I  = find(e(5,:) == 3); % Randsegment 3
RDW_3(:,1) = [e(1,I(1)); 1; 0];

%RDW = [RDW_1, RDW_2, RDW_3]; % fuer Beispiel
RDW = [RDW_1, RDW_2, RDW_3,RDW_5,RDW_6];

