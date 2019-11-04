function driver_parallel_pid
% DRIVER_PARALLEL_PID
%
% Example script for imfil.m; nonlinear least squares and parameter id.
% This generates the two figures in the section "Looking at the Parallel
% Results".
%
% C. T. Kelley, May 23, 2010
%
% This code comes with no guarantee or warranty of any kind.
%
% Solving the PID problem with implicit filering as simply as possible.
% Compare optimization/nonlinear least squares formulations with parallel
% and serial algorithms. We consider both unconstrained and constrained
% problems and a couple different choices for imfil's options.
%
% We use lots of global variables here, but plan to do this right before
% the final version. 
%
% This is an overdetermined least squares problem with n=2 and m = 100 >> n
%
% pid_parms contains the zero-residual solution to the noise-free problem
% pid_tol is the tolerance given to ode15s
%
hist_optim=cell(4,2);
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
figure(1);
%
% Constrained problem with too few scales.
%
case_number=1;
bounds=[2 20; 0 5];
x0=[5,5]'; budget= 100;
options=imfil_optset('scaledepth',5);
[histso,histsl,histpo,histpl]=run_case(x0,budget,bounds,options,pid_info);
plot_cases(histso,histsl,histpo,histpl,1);
hist_case_1=histso;
legend('serial-opt','serial-lsq','parallel-opt','parallel-lsq');
ylabel('Value of f');
title('Default scales; constraints active');
%
% Constrained problem with more scales.
%
case_number=2;
bounds=[2 20; 0 5];
x0=[5,5]'; budget= 100;
options=imfil_optset;
[histso,histsl,histpo,histpl]=run_case(x0,budget,bounds,options,pid_info);
plot_cases(histso,histsl,histpo,histpl,2);
hist_case_2=histso;
title('More scales; constraints active');
%
% Unconstrained problem with thin budget.
%
case_number=3;
bounds=[0 20; 0 5];
x0=[5,5]';
options=imfil_optset;
[histso,histsl,histpo,histpl]=run_case(x0,budget,bounds,options,pid_info);
plot_cases(histso,histsl,histpo,histpl,case_number,'semi-log');
hist_case_3=histso;
xlabel('Calls to integrator'); ylabel('Value of f');
title('Low budget; constraints inactive');
%
% Unconstrained problem with sufficient budget.
%
case_number=4;
bounds=[0 20; 0 5];
x0=[5,5]';
budget=200;
options=imfil_optset('scaledepth',20);
[histso,histsl,histpo,histpl]=run_case(x0,budget,bounds,options,pid_info);
plot_cases(histso,histsl,histpo,histpl,case_number,'semi-log');
hist_case_4=histso;
xlabel('Calls to integrator');
title('High budget; constraints inactive');
%

function [histso,histsl,histpo,histpl]=run_case(x0,budget,bounds,...
                 options,pid_info)
for pflag=0:1
for lflag=0:1
opt_case=lflag+2*pflag;
%
% Call imfil,
%
options=imfil_optset('least_squares',lflag,'parallel',pflag,options);
if lflag==0
[x,histout]=imfil(x0,@pidobj,budget,bounds,options,pid_info);
else
[x,histout]=imfil(x0,@parallel_pidlsq,budget,bounds,options,pid_info);
end
%
switch opt_case
     case 0 % serial + optimization formulation
         histso=histout;
     case 1 % serial + least squares
         histsl=histout;
     case 2 % parallel + optimization formulation
         histpo=histout;
     case 3 % parallel + least squares
         histpl=histout;
end
%
end
end


function plot_cases(histso,histsl,histpo,histpl,np,plot_form);
p1=subplot(2,2,np);
if nargin == 5
p2=plot(histso(:,1),histso(:,2),'-',histsl(:,1),histsl(:,2),'--',...
        histpo(:,1),histpo(:,2),'-o',histpl(:,1),histpl(:,2),'-x');
set(p1,'FontSize',14,'XTick',[0 50 100 150 200],'XLim',[0 100],'YLim',[20 70]);
else
p2=semilogy(histso(:,1),histso(:,2),'-',histsl(:,1),histsl(:,2),'--',...
        histpo(:,1),histpo(:,2),'-o',histpl(:,1),histpl(:,2),'-x');
set(p1,'FontSize',14,'XTick',[0 50 100 150 200],'XLim',[0 200],...
    'YLim',[1.d-4, 1.d2],'YTick',[1.d-4 1.d-2 1.d2]);
end
set(p2,'LineWidth',1.5,'Color','black');
