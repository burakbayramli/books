function [f,ifail,icount]=sa_serial_pidlsq(x,h,pid_info)
%
% Parameter ID example formulated as nonlinear least squares problem.
%
% Scale-aware version.
%
% Unpack the pid_info structure and get organized.
%
pid_data=pid_info.pid_data;
time_pts=pid_info.time_pts;
y0=pid_info.pid_y0;
%
% This scale-aware version will compute a tolerance from the
% scale. 
%
tol = .1*h*h;
%
% Call the integrator only if x is physically reasonable, ie if
% x(1) and x(2) are nonnegative. Otherwise, report a failure.
%
ifail=0; icount=1;
if min(x) < 0
   ifail=1; icount=0; f=NaN;
else
   options=odeset('RelTol',tol,'AbsTol',tol,'Jconstant',1);
   [t,y]=ode15s(@yfunsp, time_pts, y0, options, x);
   f=y(:,1)-pid_data(:,1);
end

