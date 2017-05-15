% phase of pure time delay
Td=2; %time delay in seconds
w=0.1:0.1:100; %frequency data set in rad/s
phased=-Td*w; %phase induced by time delay
plot(w,phased,'k'); %plots phased vs. w
title('phase caused by a delay of 2 seconds'); 
xlabel('rad/s'); ylabel('radians');

dtangent=(phased(1000)-phased(100))/(w(1000)-w(100)); %taking last part of the phase line
eTd=-dtangent; %Td estimate
eTd %result display


