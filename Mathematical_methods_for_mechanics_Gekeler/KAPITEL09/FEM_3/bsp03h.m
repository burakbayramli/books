function [RDZ,RDW] = bsp03h(e,Parmeter)
% flow past a cylinder,
% -- BOUNDARY CONDITIONS FOR STREAM FUNCTION

VS = Parmeter(3);
I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
E = e(:,I); q = E(1,2:LI);
RDZ_1 = [q; -5*ones(1,LI-1)];
I = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDZ_3 = [e(1,I); 5*ones(1,LI)];
I = find(e(5,:) == 4); LI = length(I); % Randsegment 4
q = [e(1,I),e(2,I(LI))];
AUX = linspace(5,-5,length(q));
RDZ_4 = [q; AUX];
I = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDZ_5 = [e(1,I);zeros(1,LI)];
I = find(e(5,:) == 6); LI = length(I); % Randsegment 6
RDZ_6 = [e(1,I); zeros(1,LI)];
I = find(e(5,:) == 7); LI = length(I); % Randsegment 7
q = [e(1,I),e(2,I(LI))];
RDZ_7 = [q; -5*ones(1,LI+1)];
I = find(e(5,:) == 9); LI = length(I); % Randsegment 9
RDZ_9 = [e(1,I); 5*ones(1,LI)];

RDZ = [RDZ_1, RDZ_3, RDZ_4, RDZ_5, RDZ_6, RDZ_7, RDZ_9];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- BOUNDARY CONDITIONS FOR VORTICITY
% TYP 1: w = w_B; TYP 2: w = w_wall;
% TYP 3: w = w_b in segments with w = w_wall
% RD_W = [[Anf.-punkte]',TYP, Wert]

I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
E = e(:,I); q = E(1,2:LI);
RDW_1 = [q; ones(1,LI-1); zeros(1,LI-1)];
I = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDW_3 = [e(1,I); ones(1,LI); zeros(1,LI)];
I = find(e(5,:) == 4); LI = length(I); % Randsegment 4
q = [e(1,I),e(2,I(LI))];
RDW_4 = [q; ones(1,LI+1); zeros(1,LI+1)];
I = find(e(5,:) == 5); LI = length(I); % Randsegment 5
RDW_5 = [e(1,I); 2*ones(1,LI); VS*ones(1,LI)];
I = find(e(5,:) == 6); LI = length(I); % Randsegment 6
RDW_6 = [e(1,I); 2*ones(1,LI); VS*ones(1,LI)];
I = find(e(5,:) == 7); LI = length(I); % Randsegment 7
q = [e(1,I),e(2,I(LI))];
RDW_7 = [q; ones(1,LI+1); zeros(1,LI+1)];
I = find(e(5,:) == 9); LI = length(I); % Randsegment 9
RDW_9 =[e(1,I); ones(1,LI); zeros(1,LI)];

RDW = [RDW_1, RDW_3, RDW_4, RDW_5, RDW_6, RDW_7, RDW_9];
