function deriv = sprrk(s,t,param)
%  Returns right-hand side of 3 mass-spring system
%  equations of motion
%  Inputs
%    s       State vector [x(1) x(2) ... v(3)]
%    t       Time (not used)
%    param   (Spring constant)/(Block mass)
%  Output
%    deriv   [dx(1)/dt dx(2)/dt ... dv(3)/dt]
deriv(1) = s(4);
deriv(2) = s(5);
deriv(3) = s(6);
param2 = -2*param;
deriv(4) = param2*s(1) + param*s(2);
deriv(5) = param2*s(2) + param*(s(1)+s(3));
deriv(6) = param2*s(3) + param*s(2);
return;
