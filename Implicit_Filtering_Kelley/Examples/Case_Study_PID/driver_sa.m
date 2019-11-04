function driver_pid
% DRIVER_PID
%
% Example script for imfil.m; nonlinear least squares and parameter id.
% C. T. Kelley, May 18, 2010.
%
% This is the example Chapter 8 of the book and Chapter 2 of the manual.
%
% This code comes with no guarantee or warranty of any kind.
%
% Solving the PID problem with implicit filering. This an example of
% a scale-aware formulation.
%
% This is an overdetermined least squares problem with n=2 and m >> n
%
% pid_parms contains the zero-residual solution to the noise-free problem
% pid_tol is the tolerance given to ode15s
%
m=100; t0=0; tf=10;
%
% Construct the data for the integration.
% pid_data is a sampling of the "true" solution.
%
pid_parms=[1,1]'; pid_y0=[10,0]'; pid_tol=1.d-3;
time_pts=(0:m)'*(tf-t0)/m+t0; 
%
% Find the analytic solution.
%
pid_data=exact_solution(time_pts,pid_y0,pid_parms);
%
% Pack the data into a structure to pass to sa_serial_pidlsq
%
pid_info=struct('pid_y0',pid_y0,'pid_tol',pid_tol,...
                'time_pts',time_pts,'pid_data',pid_data);
%
% Set the bounds, budget, initial iterate for imfil.m.
%
bounds=[0 20; 0 5]; x0=[5,5]'; budget= 200;
%
% Call imfil and turn scale_aware on.
%
options=imfil_optset('least_squares',1,'scale_aware',1,'scaledepth',20);
[x,histout]=imfil(x0,@sa_serial_pidlsq,budget,bounds,options,pid_info);
[mh,nh]=size(histout);
nd=nh-5;
%
% and make a nice plot.
%
figure(1)
p1=subplot(1,1,1);
p2=semilogy(histout(:,1),histout(:,2),'-');
set(p1,'FontSize',14,'XTick',[0 50 100 150 200],'XLim',[0 200],...
  'YLim',[1.d-9, 1.d2],'YTick',...
  [1.d-9 1.d-8 1.d-7 1.d-6 1.d-5 1.d-4 1.d-3 1.d-2 1.d-1 1.d0 1.d1 1.d2]);
set(p2,'LineWidth',1.5,'Color','black');
ylabel('Value of f');
xlabel('Calls to integrator');
title('Scale-Aware Optimization')
