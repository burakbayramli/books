function RDZ = bsp01h(e,T,Parmeter);
% Randdaten sea
A = Parmeter(1); PERIOD = Parmeter(2); omga = 2*pi/PERIOD;
I = find(e(5,:) == 1); LI = length(I); %Randsegment 1
RDZ = [[e(1,I),e(2,I(LI))]; A*sin(omga*T)*ones(1,LI+1)];
