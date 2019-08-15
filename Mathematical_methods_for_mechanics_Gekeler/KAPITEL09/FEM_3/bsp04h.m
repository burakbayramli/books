function [RDZ,RDW] = bsp04h(e,Parmeter)
% back facing step,
% -- BOUNDARY CONDITIONS FOR STREAM FUNCTION

VS = Parmeter(3);
I  = find(e(5,:) == 6); LI = length(I); % Randsegment 6
TT = linspace(0.056,0.02,LI+1); AUX = 10*(TT - 0.02);
RDZ_6 = [e(1,I); AUX(1:LI)];
I  = find(e(5,:) == 7); LI = length(I); % Randsegment 7
RDZ_7 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 8); LI = length(I); % Randsegment 8
RDZ_8 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 9); LI = length(I); % Randsegment 9
RDZ_9 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 14); LI = length(I); % Randsegment 14
RDZ_14 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 19); LI = length(I); % Randsegment 19
RDZ_19 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 24); LI = length(I); % Randsegment 24
RDZ_24 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 29); LI = length(I); % Randsegment 29
RDZ_29 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 34); LI = length(I); % Randsegment 34
RDZ_34 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 39); LI = length(I); % Randsegment 39
RDZ_39 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 44); LI = length(I); % Randsegment 44
RDZ_44 = [e(1,I); zeros(1,LI)];
I  = find(e(5,:) == 45); LI = length(I); % Randsegment 45
RDZ_45 = [e(1,I(1)); 0];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
I  = find(e(5,:) == 43); LI = length(I); % Randsegment 43
RDZ_43 = [e(1,I); 0.36*ones(1,LI)];
I  = find(e(5,:) == 38); LI = length(I); % Randsegment 38
RDZ_38 = [e(1,I); 0.36*ones(1,LI)];
I  = find(e(5,:) == 33); LI = length(I); % Randsegment 33
RDZ_33 = [e(1,I); 0.36*ones(1,LI)];
I  = find(e(5,:) == 28); LI = length(I); % Randsegment 28
RDZ_28 = [e(1,I); 0.36*ones(1,LI)];
I  = find(e(5,:) == 23); LI = length(I); % Randsegment 23
RDZ_23 = [e(1,I); 0.36*ones(1,LI)];
I  = find(e(5,:) == 18); LI = length(I); % Randsegment 18
RDZ_18 = [e(1,I); 0.36*ones(1,LI)];
I  = find(e(5,:) == 13); LI = length(I); % Randsegment 13
RDZ_13 = [e(1,I); 0.36*ones(1,LI)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDZ_3 = [e(1,I); 0.36*ones(1,LI)];
I  = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDZ_5 = [e(1,I); 0.36*ones(1,LI)];

RDZA = [RDZ_6,RDZ_7,RDZ_8,RDZ_9,RDZ_14,RDZ_19,RDZ_24,RDZ_29];
RDZB = [RDZ_34,RDZ_39,RDZ_44,RDZ_45];
RDZC = [RDZ_43,RDZ_38,RDZ_33,RDZ_28,RDZ_23,RDZ_18,RDZ_13,RDZ_3,RDZ_5];
RDZ = [RDZA,RDZB,RDZC];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- BOUNDARY CONDITIONS FOR VORTICITY
% W = 0 in der Aussenecke !!!
% TYP 1: w = w_B; TYP 2: w = w_wall,
% TYP 3: w = w_b in segments with w = w_wall
% RDW = [[Anf.-punkte]',TYP, Wert];

I  = find(e(5,:) == 6); LI = length(I); % Randsegment 6
RDW_6 = [e(1,I); ones(1,LI); zeros(1,LI)];
I  = find(e(5,:) == 7); LI = length(I); % Randsegment 7
RDW_7 = [e(1,I); 2*ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 8); LI = length(I); % Randsegment 8
RDW_8 = [e(1,I);2*ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 9); LI = length(I); % Randsegment 9
RDW_9 = [e(1,I);2*ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 14); LI = length(I); % Randsegment 14
RDW_14 = [e(1,I);2*ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 19); LI = length(I); % Randsegment 19
RDW_19 = [e(1,I);2*ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 24); LI = length(I); % Randsegment 24
RDW_24 = [e(1,I);2*ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 29); LI = length(I); % Randsegment 29
RDW_29 = [e(1,I);2*ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 34); LI = length(I); % Randsegment 34
RDW_34 = [e(1,I);2*ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 39); LI = length(I); % Randsegment 39
RDW_39 = [e(1,I);2*ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 44); LI = length(I); % Randsegment 44
RDW_44 = [e(1,I);2*ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 45); LI = length(I); % Randsegment 44
RDW_45 = [e(1,I(1));2;0];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
I  = find(e(5,:) == 43); LI = length(I); % Randsegment 43
RDW_43 = [e(1,I);ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 38); LI = length(I); % Randsegment 38
RDW_38 = [e(1,I);ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 33); LI = length(I); % Randsegment 33
RDW_33 = [e(1,I);ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 28); LI = length(I); % Randsegment 28
RDW_28 = [e(1,I);ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 23); LI = length(I); % Randsegment 23
RDW_23 = [e(1,I);ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 18); LI = length(I); % Randsegment 18
RDW_18 = [e(1,I);ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 13); LI = length(I); % Randsegment 13
RDW_13 = [e(1,I);ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDW_3 = [e(1,I);ones(1,LI);zeros(1,LI)];
I  = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDW_5 = [e(1,I);ones(1,LI);zeros(1,LI)];

% -- Additional boundary conditions for W
% -- in segments with Wbound
I  = find(e(5,:) == 9); % Randsegment 9
RDW_9(:,1) = [e(1,I(1)); 1; 0];
%I  = find(e(5,:) == 8); % Randsegment 8
%RDW_8(:,1) = [e(1,I(1)); 1; 0];

% inlet mit w = 0
RDWA = [RDW_6,RDW_7,RDW_8,RDW_9,RDW_14,RDW_19,RDW_24,RDW_29,RDW_34,RDW_39,RDW_44];
% inlet ohne w = 0;
%RDWA = [RDW_7,RDW_8,RDW_9,RDW_14,RDW_19,RDW_24,RDW_29,RDW_34,RDW_39,RDW_44];
RDWB = [RDW_45,RDW_43,RDW_38,RDW_33,RDW_28,RDW_23,RDW_18,RDW_13,RDW_3,RDW_5];
RDW = [RDWA,RDWB];
