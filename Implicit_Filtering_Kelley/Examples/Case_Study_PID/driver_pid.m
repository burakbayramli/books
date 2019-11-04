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
% Solving the PID problem with implicit filering as simply as possible.
% Nonlinear least squares formuation.
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
% Pack the data into a structure to pass to serial_pidlsq
%
pid_info=struct('pid_y0',pid_y0,'pid_tol',pid_tol,...
                'time_pts',time_pts,'pid_data',pid_data);
%
% Set the bounds, budget, initial iterate for imfil.m.
%
bounds=[2 20; 0 5];
x0=[5,5]'; budget= 100;
%
% Call imfil,
%
options=imfil_optset('scaledepth',5,'least_squares',1);
[x,histout]=imfil(x0,@serial_pidlsq,budget,bounds,options,pid_info);
[mh,nh]=size(histout);
nd=nh-5;
%
% and make a nice plot.
%
figure(1)
p1=subplot(2,2,1);
p2=plot(histout(:,1),histout(:,2),'-');
set(p1,'FontSize',14,'XTick',[0 50 100 150 200],'XLim',[0 100],'YLim',[20 70]);
set(p2,'LineWidth',1.5,'Color','black');
%xlabel('Calls to integrator'); 
ylabel('Value of f');
%xlabel('Calls to integrator'); ylabel('Value of f');
title('Few scales; constraints active');
%
% The second example uses more scales.
% 
%
options=imfil_optset;
options=imfil_optset('least_squares',1);
[x2,histout2]=imfil(x0,@serial_pidlsq,budget,bounds,options,pid_info);
%
p1=subplot(2,2,2);
axis=[0 150 20 70];
p2=plot(histout2(:,1),histout2(:,2),'-');
set(p1,'FontSize',14,'XTick',[0 50 100 150 200],'XLim',[0 100],'YLim',[20 70]);
set(p2,'LineWidth',1.5,'Color','black');
%xlabel('Calls to integrator'); ylabel('Value of f');
title('Default scales; constraints active');
%
% Now fix the bounds so that the exact answer is inside the hyperrectangle.
% These bounds include points where the hidden constraints are violated.
% The objective function will complain when this happens, but imfil.m
% will happily keep working.
%
% The budget is a little light for this example.
%
bounds=[0 20; 0 5];
x0=[5,5]';
options=imfil_optset;
options=imfil_optset('scaledepth',20,'least_squares',1);
[x2,histout2]=imfil(x0,@serial_pidlsq,budget,bounds,options,pid_info);
%
p1=subplot(2,2,3);
p2=semilogy(histout2(:,1),histout2(:,2),'-');
set(p1,'FontSize',14,'XTick',[0 50 100 150 200],'XLim',[0 200],...
  'YLim',[1.d-4, 1.d2],'YTick',[1.d-4 1.d-3 1.d-2 1.d-1 1.d0 1.d1 1.d2]);
set(p2,'LineWidth',1.5,'Color','black');
xlabel('Calls to integrator'); ylabel('Value of f');
title('Low budget; constraints inactive');
%
% This is the same example as the one above, with a healthier budget.
%
budget=200;
options=imfil_optset('scaledepth',20,'least_squares',1);
[x2,histout2]=imfil(x0,@serial_pidlsq,budget,bounds,options,pid_info);
%
p1=subplot(2,2,4);
p2=semilogy(histout2(:,1),histout2(:,2),'-');
set(p1,'FontSize',14,'XTick',[0 50 100 150 200],'XLim',[0,200],...
  'YLim',[1.d-4, 1.d2],'YTick',[1.d-4 1.d-3 1.d-2 1.d-1 1.d0 1.d1 1.d2]);
set(p2,'LineWidth',1.5,'Color','black');
xlabel('Calls to integrator'); 
title('High budget; constraints inactive');
