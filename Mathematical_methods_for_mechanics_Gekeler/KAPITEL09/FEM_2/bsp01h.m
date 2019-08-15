function [RDU,RDV,LASTENU,LASTENV] = bsp01h(e);
% Beispiel Schwarz, S. 190,191, beisp68.dat
% Spanner
% fuer kubische Elemente

% Randdaten fuer U
I = find(e(5,:) == 3); LI = length(I); % boundary 3
E = [e(1,I),e(2,I(LI))];
RDU3 = [E;zeros(1,LI+1)];

I = find(e(5,:) == 5); LI = length(I); % boundary 5
E = [e(1,I),e(2,I(LI))];
RDU5 = [E;zeros(1,LI+1)];
RDU = [RDU3, RDU5];
% Randaten fuer V
I = find(e(5,:) == 3); LI = length(I); % boundary 3
E = [e(1,I),e(2,I(LI))];
RDV3 = [E;zeros(1,LI+1)];

I = find(e(5,:) == 5); LI = length(I); % boundary 5
E = [e(1,I),e(2,I(LI))];
RDV5 = [E;zeros(1,LI+1)];
RDV = [RDV3, RDV5];

% Lasten fuer U
I = find(e(5,:) == 1); LI = length(I); % boundary 1
E = [e(1,I),e(2,I(LI))];
LU1 = [E;-3*20*ones(1,LI+1)/(LI+1)];

LASTENU = LU1;

% Lasten fuer  V
I = find(e(5,:) == 1); LI = length(I); % boundary 1
E = [e(1,I),e(2,I(LI))];
LV1 = [E;-3*70*ones(1,LI+1)/(LI+1)];

LASTENV = LV1;
