function [x,histout,chistout,neval]= history_test
% HISTORY_TEST
%
% Example script for imfil.m; nonlinear least squares and parameter id.
% Plot some things from the complete_history structure.
% This generates the final figure in the "Looking at the Parallel Results"
% section
%
% C. T. Kelley, July 21, 2009
%
% This code comes with no guarantee or warranty of any kind.
%
% Solving the PID problem with implicit filering as simply as possible.
% Compare optimization/nonlinear least squares formulations with parallel
% and serial algorithms. We consider both unconstrained and constrained
% problems and a couple different choices for imfil's options.
%
% We use lots of global variables here to avoid pain
%
% This is an overdetermined least squares problem with n=2 and m = 100 >> n
%
% pid_parms contains the zero-residual solution to the noise-free problem
% pid_tol is the tolerance given to ode15s
%
isr1=0;
pflag=0;
hist_optim=cell(4,2);
m=100; t0=0; tf=10;
%
% Set the globals for the objective.
% pid_data is a sampline of the "true" solution.
%
pid_parms=[1,1]'; pid_y0=[10,0]'; pid_tol=1.d-3;
time_pts=(0:m)'*(tf-t0)/m+t0;
%
pid_data=exact_solution(time_pts,pid_y0,pid_parms);
%
% Pack the data into a structure to pass to serial_pidlsq
%
pid_info=struct('pid_y0',pid_y0,'pid_tol',pid_tol,...
                'time_pts',time_pts,'pid_data',pid_data);
%
bounds=[2 20; 0 5];
bounds=[0 5; 0 5];
x0=[5,5]'; budget= 400;
neval=zeros(2,1);
for il=0:1
    options=imfil_optset('least_squares',1,'parallel',il);
    [x,histout,chistout]=imfil(x0,@parallel_pidlsq,budget,...
                  bounds,options,pid_info);
    [mh,nh]=size(histout);
    neval(il+1)=histout(mh,1);
    figure(1)
    subplot(1,2,il+1)
    [mv,nv]=size(chistout.good_values);
    p1=subplot(1,2,il+1);
    p2=plot(chistout.good_points(1,:),chistout.good_points(2,:),'*');
    axis('square');
    if il==0
       title('Serial');
    else
       title('Parallel');
    end
    set(p1,'FontSize',14,'XLim',bounds(1,:),'YLim',bounds(2,:));
end
print -deps history_ch1

