function compEMRK4
% compEMRK4  Compare flops and accuracy of Euler, Midpoint and RK4 methods
%            for the solution of  dy/dt = -y;  y(0) = 1
%
% Synopsis:  compEMRK4
%
% Input:     none
%
% Output:    Flops and global trunc errors for a sequence of stepsizes

tn = 1;  y0 = 1;           %  Length of interval and initial condition

fprintf('\n   h       flopsE    errE      flopsM    errM      flops4    err4\n');
for h = [0.2  0.1  0.05  0.025  0.0125  0.00625]
  flops(0);  [te,ye] = odeEuler('rhs2',tn,h,1);   flopse = flops;  %  Euler solution
  flops(0);  [tm,ym] = odeMidpt('rhs2',tn,h,1);   flopsm = flops;  %  Midpoint solution
  flops(0);  [t4,y4] = odeRK4('rhs2',tn,h,1);     flops4 = flops;  %  RK4 solution
  % --- global discretization errors
  yex = y0*exp(-te);                     %  Exact solution at discrete t
  erre = max(abs(ye-yex));  errm = max(abs(ym-yex));   err4 = max(abs(y4-yex));
  fprintf('%8.5f %7d %11.2e %7d %11.2e %7d %11.2e\n',...
           h,flopse,erre,flopsm,errm,flops4,err4);
end
