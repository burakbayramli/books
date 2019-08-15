function [RDU,RDV,RDZ] = bsp01h(e,T,Parmeter);
% island in a bay, boundary data 
% 
A = Parmeter(1); PERIOD = Parmeter(2); omga = 2*pi/PERIOD;

I = find(e(5,:) == 1); LI = length(I); % seg. nr. 1, sea
RDZ = [[e(1,I),e(2,I(LI))]; A*sin(omga*T)*ones(1,LI+1)];

I = find(e(5,:) == 2); LI = length(I); % seg. nr. 2, land
RDU = [e(1,I(2:LI)); zeros(1,LI-1)];

I = find(e(5,:) == 3); LI = length(I); % seg. nr. 3, island
RDV = [e(1,I),e(2,I(LI)); zeros(1,LI+1)];
