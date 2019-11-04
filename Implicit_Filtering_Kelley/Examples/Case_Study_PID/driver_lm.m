function [histoutl, histoutg, histoutlx, histoutgx]=driver_lm
% DRIVER_LM
%
% Test for the executive_function option. Solve the PID problem with
% a Levenberg-Marquardt iteration.
%
% C. T. Kelley, Jan 24, 2011.
%
% This code comes with no guarantee or warranty of any kind.
%
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
x0=[5,5]'; budget= 50;
%
% Call imfil. Global minimum out of bounds.
%
options=imfil_optset('least_squares',1,...
   'executive_function',@lev_mar_exec,'executive_data',1.0);
[x,histoutl]=imfil(x0,@serial_pidlsq,budget,bounds,options,pid_info);
options=imfil_optset('least_squares',1);
[x,histoutg]=imfil(x0,@serial_pidlsq,budget,bounds,options,pid_info);
%
% Call imfil. Global minimum within bounds.
%
bounds=[0 20; 0 5];
budget=200;
options=imfil_optset('least_squares',1,'scaledepth',20,...
   'executive_function',@lev_mar_exec,'executive_data',1.0);
[x,histoutlx]=imfil(x0,@serial_pidlsq,budget,bounds,options,pid_info);
options=imfil_optset('least_squares',1,'scaledepth',20);
[x,histoutgx]=imfil(x0,@serial_pidlsq,budget,bounds,options,pid_info);
%
% Left side plot.
%
figure(1)
p1=subplot(1,2,1);
p2=plot(histoutg(:,1),histoutg(:,2),'-',histoutl(:,1),histoutl(:,2),'--');
axis('square')
set(p1,'FontSize',14,'XLim',[0 50],'YLim',[20 70]);
set(p2,'LineWidth',1.5,'Color','black');
legend('G-N','L-M');
xlabel('Calls to integrator'); 
ylabel('Value of f');
title('Constraints Active');
%
% Right side plot.
%
p1=subplot(1,2,2);
p2=semilogy(histoutgx(:,1),histoutgx(:,2),'-',...
           histoutlx(:,1),histoutlx(:,2),'--');
set(p1,'FontSize',14,'XTick',[0 50 100 150 200],...
            'XLim',[0 200],'YLim',[1.d-3 100],'YTick',[1.d-3 1.d-1 1.d2]);
axis('square')
set(p2,'LineWidth',1.5,'Color','black');
title('Constraints Inactive');
%legend('Gauss-Newton','Levenberg-Marquardt');
xlabel('Calls to integrator');
%ylabel('Value of f');

