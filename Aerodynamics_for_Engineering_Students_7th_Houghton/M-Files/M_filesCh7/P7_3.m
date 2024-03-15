%
% Chapter 5 Problem 3.
CL = -.5:.01:.5;
delta = 0;
deltad = 0;
AR = 7.63;
CDV = (1+delta)*CL.^2./(pi*AR);
CDVd = (1+deltad)*CL.^2./(pi*AR); 
plot(CL,CDV,'k')
title('Elliptic load distribution; AR = 7.63')
ylabel('Induced drag coefficient, C_{Dv}')
xlabel('Lift coefficient, C_L')