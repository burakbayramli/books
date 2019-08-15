function [RDU,RDV,RDZ] = bsp02h(e,T,Parmeter);
%1. Zeile: Knoten-Nrn, 2. Zeile: Randwerte, 3. Zeile: Ableitungen
A = Parmeter(2); PERIOD = Parmeter(5);
omga = 2*pi/PERIOD;
% -- U-Teil ----------
I = find(e(5,:) == 2); LI = length(I); % Randsegment 2
RDU_2 = [e(1,I), e(2,I(LI)); zeros(1,LI+1); zeros(1,LI+1)];
RDU = RDU_2;
% -- V-Teil,-- wichtig in dieser Form!! ----------- 
I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
RDV_1 = [e(1,I); zeros(1,LI); zeros(1,LI)];
I = find(e(5,:) == 3); LI = length(I); % Randsegment 3
RDV_3 = [e(1,I); zeros(1,LI); zeros(1,LI)];
RDV = [RDV_1,RDV_3];
% -- Z-Teil ---------------
I = find(e(5,:) == 4); LI = length(I); % Randsegment 4
RDZ_4 = [e(1,I), e(2,I(LI));A*sin(omga*T)*ones(1,LI+1);
        (A*omga)*cos(omga*T)*ones(1,LI+1)];
RDZ = RDZ_4;
