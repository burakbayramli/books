function [r,ru,rv,E,FinalTime] = Riemann2D(x,y,gamma,RiemannProbCase);
% function [r,ru,rv,E,FInalTime] = Riemann2D(x,y,gamma,RiemannProbCase);
% Purpose: Initialize 2D Riemann problem. 
% Reference: P. Lax and X.-D. Liu, Solution of two-dimensional Riemann problems 
% of gas dynamics by positive schemes, SIAM J. Sci. Comp., 19 (1998), 319?340.

if (RiemannProbCase==1) % 4 rarefication waves
    r1= 1.0000; u1= 0.0000; v1= 0.0000; p1= 1.0000; 
    r2= 0.5197; u2=-0.7259; v2= 0.0000; p2= 0.4000;
    r3= 0.1072; u3=-0.7259; v3=-1.4045; p3= 0.0439; 
    r4= 0.2579; u4= 0.0000; v4=-1.4045; p4= 0.1500;
    FinalTime = 0.2;
elseif (RiemannProbCase==2) % 4 Rarefication waves
    r1= 1.0000; u1= 0.0000; v1= 0.0000; p1= 1.0000; 
    r2= 0.5197; u2=-0.7259; v2= 0.0000; p2= 0.4000;
    r3= 1.0000; u3=-0.7259; v3=-0.7259; p3= 1.0000; 
    r4= 0.5197; u4= 0.0000; v4=-0.7259; p4= 0.4000;
    FinalTime = 0.2;
elseif (RiemannProbCase==3) % 4 shocks
    r1= 1.5000; u1= 0.0000; v1= 0.0000; p1= 1.5000; 
    r2= 0.5323; u2= 1.2060; v2= 0.0000; p2= 0.3000;
    r3= 0.1380; u3= 1.2060; v3= 1.2060; p3= 0.0290; 
    r4= 0.5323; u4= 0.0000; v4= 1.2060; p4= 0.3000;
    FinalTime = 0.3;
elseif (RiemannProbCase==4) % 4 shocks
    r1= 1.1000; u1= 0.0000; v1= 0.0000; p1= 1.1000; 
    r2= 0.5065; u2= 0.8939; v2= 0.0000; p2= 0.3500;
    r3= 1.1000; u3= 0.8939; v3= 0.8939; p3= 1.1000; 
    r4= 0.5065; u4= 0.0000; v4= 0.8939; p4= 0.3500;
    FinalTime = 0.25;
elseif (RiemannProbCase==5) % 4 contact waves
    r1= 1.0000; u1=-0.7500; v1=-0.5000; p1= 1.0000; 
    r2= 2.0000; u2=-0.7500; v2= 0.5000; p2= 1.0000;
    r3= 1.0000; u3= 0.7500; v3= 0.5000; p3= 1.0000; 
    r4= 3.0000; u4= 0.7500; v4=-0.5000; p4= 1.0000;
    FinalTime = 0.23;
elseif (RiemannProbCase==6) % 4 contact waves
    r1= 1.0000; u1= 0.7500; v1=-0.5000; p1= 1.0000; 
    r2= 2.0000; u2= 0.7500; v2= 0.5000; p2= 1.0000;
    r3= 1.0000; u3=-0.7500; v3= 0.5000; p3= 1.0000; 
    r4= 3.0000; u4=-0.7500; v4=-0.5000; p4= 1.0000;
    FinalTime = 0.3;
elseif (RiemannProbCase==7) % 2 contact waves, 2 rarefication waves
    r1= 1.0000; u1= 0.1000; v1= 0.1000; p1= 1.0000; 
    r2= 0.5197; u2=-0.6259; v2= 0.1000; p2= 0.4000;
    r3= 0.8000; u3= 0.1000; v3= 0.1000; p3= 0.4000; 
    r4= 0.5197; u4= 0.1000; v4=-0.6259; p4= 0.4000;
    FinalTime = 0.25;
elseif (RiemannProbCase==8) % 2 contact waves, 2 rarefication waves
    r1= 0.5197; u1= 0.1000; v1= 0.1000; p1= 0.4000; 
    r2= 1.0000; u2=-0.6259; v2= 0.1000; p2= 1.0000;
    r3= 0.8000; u3= 0.1000; v3= 0.1000; p3= 1.0000; 
    r4= 1.0000; u4= 0.1000; v4=-0.6259; p4= 1.0000;
    FinalTime = 0.25;
elseif (RiemannProbCase==9) % 2 contact waves, 2 rarefication waves
    r1= 1.0000; u1= 0.0000; v1= 0.3000; p1= 1.0000; 
    r2= 2.0000; u2= 0.0000; v2=-0.3000; p2= 1.0000;
    r3= 1.0390; u3= 0.0000; v3=-0.8133; p3= 0.4000; 
    r4= 0.5197; u4= 0.0000; v4=-0.4259; p4= 0.4000; 
    FinalTime = 0.3;
elseif (RiemannProbCase==10) % 2 contact waves, 2 rarefication waves
    r1= 1.0000; u1= 0.0000; v1= 0.4297; p1= 1.0000; 
    r2= 0.5000; u2= 0.0000; v2= 0.6076; p2= 1.0000;
    r3= 0.2281; u3= 0.0000; v3=-0.6076; p3= 0.3333; 
    r4= 0.4562; u4= 0.0000; v4=-0.4297; p4= 0.3333; 
    FinalTime = 0.15;
elseif (RiemannProbCase==11) % 2 contact waves, 2 shocks
    r1= 1.0000; u1= 0.1000; v1= 0.0000; p1= 1.0000; 
    r2= 0.5313; u2= 0.8276; v2= 0.0000; p2= 0.4000;
    r3= 0.8000; u3= 0.1000; v3= 0.0000; p3= 0.4000; 
    r4= 0.5313; u4= 0.1000; v4= 0.7276; p4= 0.4000; 
    FinalTime = 0.3;
elseif (RiemannProbCase==12) % 2 contact waves, 2 shocks
    r1= 0.5313; u1= 0.0000; v1= 0.0000; p1= 0.4000; 
    r2= 1.0000; u2= 0.7276; v2= 0.0000; p2= 1.0000; % CHECK
    r3= 0.8000; u3= 0.0000; v3= 0.0000; p3= 1.0000; 
    r4= 1.0000; u4= 0.0000; v4= 0.7276; p4= 1.0000; 
    FinalTime = 0.25;
elseif (RiemannProbCase==13) % 2 contact waves, 2 shocks
    r1= 1.0000; u1= 0.0000; v1=-0.3000; p1= 1.0000; 
    r2= 2.0000; u2= 0.0000; v2= 0.3000; p2= 1.0000;
    r3= 1.0625; u3= 0.0000; v3= 0.8145; p3= 0.4000; 
    r4= 0.5313; u4= 0.0000; v4= 0.4276; p4= 0.4000; 
    FinalTime = 0.3;
elseif (RiemannProbCase==14) % 2 contact waves, 2 shocks
    r1= 2.0000; u1= 0.0000; v1=-0.5606; p1= 8.0000; 
    r2= 1.0000; u2= 0.0000; v2=-1.2172; p2= 8.0000;
    r3= 0.4736; u3= 0.0000; v3= 1.2172; p3= 2.6667; 
    r4= 0.9474; u4= 0.0000; v4= 1.1606; p4= 2.6667; 
    FinalTime = 0.1;
elseif (RiemannProbCase==15) % 2 contact waves, 1 shock, 1 rarefication wave
    r1= 1.0000; u1= 0.1000; v1=-0.3000; p1= 1.0000; 
    r2= 0.5197; u2=-0.6259; v2=-0.3000; p2= 0.4000;
    r3= 0.8000; u3= 0.1000; v3=-0.3000; p3= 0.4000; 
    r4= 0.5313; u4= 0.1000; v4= 0.4276; p4= 0.4000; 
    FinalTime = 0.2;
elseif (RiemannProbCase==16) % 2 contact waves, 1 shock, 1 rarefication wave
    r1= 0.5313; u1= 0.1000; v1= 0.1000; p1= 0.4000; 
    r2= 1.0222; u2=-0.6179; v2= 0.1000; p2= 1.0000;
    r3= 0.8000; u3= 0.1000; v3= 0.1000; p3= 1.0000; 
    r4= 1.0000; u4= 0.1000; v4= 0.8276; p4= 1.0000; 
    FinalTime = 0.2;
elseif (RiemannProbCase==17) % 2 contact waves, 1 shock, 1 rarefication wave
    r1= 1.0000; u1= 0.0000; v1=-0.4000; p1= 1.0000; 
    r2= 2.0000; u2= 0.0000; v2=-0.3000; p2= 1.0000;
    r3= 1.0625; u3= 0.0000; v3= 0.2145; p3= 0.4000; 
    r4= 0.5197; u4= 0.0000; v4=-1.1259; p4= 0.4000;
    FinalTime = 0.3;
elseif (RiemannProbCase==18) % 2 contact waves, 1 shock, 1 rarefication wave
    r1= 1.0000; u1= 0.0000; v1= 1.0000; p1= 1.0000; 
    r2= 2.0000; u2= 0.0000; v2=-0.3000; p2= 1.0000;
    r3= 1.0625; u3= 0.0000; v3= 0.2145; p3= 0.4000; 
    r4= 0.5197; u4= 0.0000; v4= 0.2741; p4= 0.4000;
    FinalTime = 0.2;
elseif (RiemannProbCase==19) % 2 contact waves, 1 shock, 1 rarefication wave
    r1= 1.0000; u1= 0.0000; v1= 0.3000; p1= 1.0000; 
    r2= 2.0000; u2= 0.0000; v2=-0.3000; p2= 1.0000;
    r3= 1.0625; u3= 0.0000; v3= 0.2145; p3= 0.4000; 
    r4= 0.5197; u4= 0.0000; v4=-0.4259; p4= 0.4000;
    FinalTime = 0.2;
else % Incorrect initial conditions
    print('Initial condition undefined');
   r1= 0.0000; u1= 0.0000; v1= 0.0000; p1= 0.0000; 
   r2= 0.0000; u2= 0.0000; v2= 0.0000; p2= 0.0000;
   r3= 0.0000; u3= 0.0000; v3= 0.0000; p3= 0.0000; 
   r4= 0.0000; u4= 0.0000; v4= 0.0000; p4= 0.0000; 
   FinalTime = 0.0;
end;

% Set up initial conditions
q1 = (x>=0.5).*(y>=0.5); q2 = (x<0.5).*(y>=0.5);
q3 = (x<0.5).*(y<0.5); q4 = (x>=0.5).*(y<0.5);

r  = r1*q1 + r2*q2 + r3*q3 + r4*q4;
ru = r1*u1*q1 + r2*u2*q2 + r3*u3*q3 + r4*u4*q4;
rv = r1*v1*q1 + r2*v2*q2 + r3*v3*q3 + r4*v4*q4;
E = (p1/(gamma-1)+0.5*r1*(u1^2+v1^2))*q1 + (p2/(gamma-1)+0.5*r2*(u2^2+v2^2))*q2 ...
    + (p3/(gamma-1)+0.5*r3*(u3^2+v3^2))*q3 + (p4/(gamma-1)+0.5*r4*(u4^2+v4^2))*q4;
return
