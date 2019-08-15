function [RDU,RDV,LASTENU,LASTENV] = bsp02h(e);
% Vgl. Beispiel Schwarz, S. 190,191, beisp68.dat
% Spanner fuer kubische Elemente
% -- Lager -----------------------------
I = find(e(5,:) == 28); LI = length(I); % boundary 28
E = [e(1,I),e(2,I(LI))];
RDU_28 = [E;zeros(1,LI+1)]; RDV_28 = RDU_28;
I = find(e(5,:) == 37); LI = length(I); % boundary 37
E = [e(1,I),e(2,I(LI))];
RDU_37 = [E;zeros(1,LI+1)]; RDV_37 = RDU_37;
RDU = [RDU_28, RDU_37];
RDV = RDU;

% -- Lasten ---------------------------
I  = find(e(5,:) == 3); LI = length(I); % boundary 3
E3 = [e(1,I),e(2,I(LI))];
I  = find(e(5,:) == 7); LI = length(I); % boundary 7
E7 = [e(1,I),e(2,I(LI))];
E  = [E3,E7];
LASTENU = [E;-3*20*ones(1,length(E))/length(E)];
LASTENV = [E;-3*70*ones(1,length(E))/length(E)];
