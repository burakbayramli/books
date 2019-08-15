function [RDU,RDV,RDZ,RCZ] = bsp01h2(e,T,A,PERIOD);
% boundary data sea
omga = 2*pi/PERIOD;
RDU = [];
RDV = [];
I = find(e(5,:) == 1); LI = length(I); %Randsegment 1
RDZ = [[e(1,I),e(2,I(LI))]; A*sin(omga*T)*ones(1,LI+1)];

% -- Cauchy BC fuer Z ----------------------
I    = find(e(5,:) == 4); LI = length(I); % Boundary 4
RCZ_4  = e(1:2,I); 
RCZ = RCZ_4;
