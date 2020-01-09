% Solves piecewise linear minimization problem
%     minimize   max_i=1..m (a_i'x + b_i)
% using subgradient method with different speed-up modifications.
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
%

%********************************************************************
% generate a problem instance
%********************************************************************
n = 20; % number of variables
m = 100; % number of terms
randn('state',1); % set state so problem is reproducable
A = randn(m,n);
b = randn(m,1);

%********************************************************************
% compute optimal value by solving a linear program (use CVX)
%********************************************************************
cvx_begin
  variable x_min(n)
  minimize( max(A*x_min + b) )
cvx_end

f_min = cvx_optval;
fprintf(1,'Optimal value is %0.4f.\n\n',f_min);

% initial point
x_1 = zeros(n,1);

%********************************************************************
% subgradient method with speed-ups
%********************************************************************
MAX_ITERS = 2000; 

% run subgradient method with diminishing step sizes
[x,hist1] = sgm_pwl_nonsum_dimin(A,b,x_1,0.1,MAX_ITERS); 
[x,hist2] = sgm_pwl_sqrsum_nonsum(A,b,x_1,1,MAX_ITERS); 
% [x,hist4] = sgm_pwl_momentum(A,b,x_1,.75,MAX_ITERS); 

% CFM speedup
[x,hist3] = sgm_pwl_cfm(A,b,x_1,f_min,MAX_ITERS); 

% filtered optimal Polyak's step
[x,hist2] = sgm_pwl_filt_opt_step(A,b,x_1,f_min,0.275,MAX_ITERS); 
[x,hist4] = sgm_pwl_filt_opt_step(A,b,x_1,f_min,0.25,MAX_ITERS); 

% run subgradient method with Polyak's optimal step
[x,histo] = sgm_pwl_optimal_step(A,b,x_1,f_min,MAX_ITERS); 

% setup plot data
iters = [1:MAX_ITERS];
f1 = hist1{1}; fbest1 = hist1{2};
f2 = hist2{1}; fbest2 = hist2{2};
f3 = hist3{1}; fbest3 = hist3{2};
f4 = hist4{1}; fbest4 = hist4{2};
fo = histo{1}; fbesto = histo{2};

figure(1), clf
set(gca, 'FontSize',18);
% semilogy( iters, fbest2-f_min, 'b:','LineWidth',1.5 ), hold on
semilogy( iters, fbesto-f_min, 'k--','LineWidth',1.5 ), hold on
semilogy( iters, fbest4-f_min, 'b','LineWidth',1.5 ), hold on
semilogy( iters, fbest3-f_min, 'g-.','LineWidth',1.5 );
% semilogy( iters, fbest1-f_min, 'b--','LineWidth',1.5 );
hold off
xlabel('k');
ylabel('fbest - fmin');
legend('optimal','with filter beta val','CFM',1);
%print -depsc pwl_speedup_step_fbest
