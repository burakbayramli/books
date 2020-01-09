% Subgradient method for the dual problem.
%
% Primal QCQP problem:
%   minimize    (1/2)x'Px - q'x 
%   subject to  x^2 <= 1
%
% where P is a positive definite matrix (objective is strictly convex)
%
% Dual problem is:
%   maximize    -(1/2)q'(P + diag(2*lambda))^{-1}q - sum(lambda)
%   subject to  lambda => 0
%
% where lambda are dual variables
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
%

% generate a problem instance
n = 50; % number of variables n
% generate problem data (P should be positive definite)
randn('state',1); % set state so problem is reproducable
A = randn(n,n);
P = (A'*A);
q = randn(n,1);
eigsP = eig( P );
fprintf(1,'Constructed mtx with min eig = %3.4f and max eig = %3.4f\n',...
        min(eigsP), max(eigsP));

%********************************************************************
% optimal solution of the primal QCQP problem
%********************************************************************
cvx_begin
  variable x(n,1);
  dual variable lambda_opt;
  minimize( 1/2*x'*P*x - q'*x )
  subject to
    x.^2 <= 1 : lambda_opt;
cvx_end

f_min = cvx_optval;
fprintf(1,'QCQP optimal value is %0.4f.\n\n',f_min);

%********************************************************************
% projected subgradient method applied to the primal problem
%********************************************************************
fp = [+Inf]; fpbest = [+Inf];
disp('Starting projected subgradient algorithm for the primal problem...')

% initial point
x = zeros(n,1);

k = 1;
MAX_ITERS = 100;

while k < MAX_ITERS 

  % subgradient calculation
  g = P*x - q;

  % primal objective values
  fval = (1/2)*x'*P*x - q'*x;
  fp(end+1) = fval; fpbest(end+1) = min( fval, fpbest(end) );

  % step size selection
  alpha = (fval-f_min)/norm(g)^2;

  % projected subgradient update
  x = x - alpha*g;  k = k + 1;

  % projection onto the feasible set (saturation function)
  x = max( min( x, 1 ), -1 ); 

end

%********************************************************************
% subgradient method applied to the dual problem
%********************************************************************
f = [+Inf]; fbest = [+Inf];
g = [-Inf]; gbest = [-Inf];
disp('Starting the subgradient algorithm applied to the dual problem...')

% initial point
lambda_1 = ones(n,1);
lambda = lambda_1;

k = 1;

while k < MAX_ITERS 

  % subgradient calculation
  x_star = (P + diag(2*lambda))\q;
  h = x_star.^2 - 1;

  % dual objective values
  gval = -(1/2)*q'*x_star - sum(lambda);
  g(end+1) = gval; gbest(end+1) = max( gval, gbest(end) );

  % primal objective values
  x_star = max( min( x_star, 1 ), -1 ); % find nearby feasible point
  fval = (1/2)*x_star'*P*x_star - q'*x_star;
  f(end+1) = fval; fbest(end+1) = min( fval, fbest(end) );

  % step size selection
  alpha = 0.1;

  % projected subgradient update
  lambda = max(0, lambda + alpha*h);  k = k + 1;

end

%********************************************************************
% plot results
%********************************************************************
figure(1), clf
set(gca, 'FontSize',18);
semilogy( [1:MAX_ITERS],fbest-gbest, 'b-', 'LineWidth',1.5 ), hold on,
semilogy( [1:MAX_ITERS],fbest-f_min, 'r-', 'LineWidth',1.5 ), hold on,
semilogy( [1:MAX_ITERS],fpbest-f_min,'k-', 'LineWidth',1.5 ), hold on,
xlabel('k');
ylabel('convergence');
legend('duality gap','optim gap dual','optim gap primal',1);

figure(2), clf
set(gca, 'FontSize',18);
plot( [1:MAX_ITERS], g, 'b-', 'LineWidth',1.5 ), hold on,
plot( [1:MAX_ITERS], f, 'g--', 'LineWidth',1.5 ), hold on,
xlabel('k');
ylabel('best values');
axis([1 40 -50 0]);
legend('gval label','fval label',4);
%print -depsc dual_prob_subgrad_conv

figure(3), clf
set(gca, 'FontSize',18);
plot( [1:MAX_ITERS], gbest, 'b-', 'LineWidth',1.5 ), hold on,
plot( [1:MAX_ITERS], fbest, 'g--', 'LineWidth',1.5 ), hold on,
xlabel('k');
ylabel('best values');
axis([1 40 -50 0]);
legend('gbest label','fbest label',4);
