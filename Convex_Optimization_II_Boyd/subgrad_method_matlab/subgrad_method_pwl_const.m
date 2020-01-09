% Solves piecewise linear minimization problem
%     minimize   max_i=1..m (a_i'x + b_i)
% using subgradient method with constant step lengths.
%
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
Rtrue = norm( x_1 - x_min );
R = 10; % we use a much bigger R than it is needed (heuristic)

%********************************************************************
% constant step length examples
%********************************************************************
TOL = 1e-3;
MAX_ITERS = 3000; 
gammas = [.05 .01 .005];

% run subgradient method with constant step length for different gammas
[x,hist1] = sgm_pwl_const_step_length(A,b,x_1,R,gammas(1),TOL,MAX_ITERS); 
[x,hist2] = sgm_pwl_const_step_length(A,b,x_1,R,gammas(2),TOL,MAX_ITERS); 
[x,hist3] = sgm_pwl_const_step_length(A,b,x_1,R,gammas(3),TOL,MAX_ITERS); 

%********************************************************************
% generate plots
%********************************************************************
% setup plot data
iters = [1:MAX_ITERS];
f1 = hist1{1}; fbest1 = hist1{2}; lbest1 = hist1{3};
f2 = hist2{1}; fbest2 = hist2{2}; lbest2 = hist2{3};
f3 = hist3{1}; fbest3 = hist3{2}; lbest3 = hist3{3};

% plots
figure(1), clf
iter_sm = 100;
set(gca, 'FontSize',18);
semilogy( iters(1:iter_sm), f1(1:iter_sm)-f_min, 'r-.','LineWidth',1.5 ); hold on;
semilogy( iters(1:iter_sm), f2(1:iter_sm)-f_min, 'g--','LineWidth',1.5 );
semilogy( iters(1:iter_sm), f3(1:iter_sm)-f_min, 'b-','LineWidth',1.5 ); hold off 
xlabel('k');
ylabel('f - fmin');
axis([1 100 1e-1 2e0]);
legend('gamma1val','gamma2val','gamma3val', 1);
%print -depsc pwl_const_step_length_fvals

figure(2), clf
set(gca, 'FontSize',18);
semilogy( iters, fbest1-f_min, 'r-.','LineWidth',1.5 ); hold on;
semilogy( iters, fbest2-f_min, 'g--','LineWidth',1.5 );
semilogy( iters, fbest3-f_min, 'b-','LineWidth',1.5 ); hold off 
xlabel('k');
ylabel('fbest - fmin');
axis([1 3000 1e-3 2e0]);
legend('gamma1val','gamma2val','gamma3val', 1);
%print -depsc pwl_const_step_length_fbest
