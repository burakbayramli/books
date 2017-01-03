function f = fBortz(rho,omega);
%
% Computes the Bortz "noncommutative rate vector" as a function of 
% measured body rates (omega) and cumulative rotation vector (rho).
%
% Generated using Maple Matlab CodeGeneration with 'optimize' option
%
t1   =  rho(3)*omega(2);
t3   =  rho(2)*omega(3);
t5   =  rho(1)^2;
t6   =  rho(2)^2;
t7   =  rho(3)^2;
t8   =  t5 + t6 + t7;
t9   =  sqrt(t8);
t10  =  sin(t9);
t12  =  cos(t9);
t19  =  (1 - t9 * t10 / (1 - t12) / 2) / t8;
t20  =  rho(3) * omega(1);
t21  =  rho(1) * omega(3);
t22  =  t20 - t21;
t24  =  rho(2) * omega(1);
t25  =  rho(1) * omega(2);
t26  = -t24 + t25;
t33  = -t1 + t3;
f(1) = -t1 / 2 + t3 / 2 + t19 * (-rho(3) * t22 + rho(2) * t26);
f(2) =  t20 / 2 - t21 / 2 + t19 * (rho(3) * t33 - rho(1) * t26);
f(3) = -t24 / 2 + t25 / 2 + t19 * (-rho(2) * t33 + rho(1) * t22);
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  