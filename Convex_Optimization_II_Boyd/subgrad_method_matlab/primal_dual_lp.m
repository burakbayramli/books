% Solve linear program
%   minimize    c'*x
%   subject to  Ax <= b
% using primal-dual subgradient method
%
% generates plot of best objective value versus iterations
%
% EE364b Convex Optimization II, S. Boyd
% Written by Jaehyun Park, May 2014
%

% generate a problem instance
n = 20; % number of variables
m = 200; % number of inequalities
% set state so problem is reproducible
randn('state', 1);
rand('state', 1);
A = randn(m, n);
b = rand(m, 1);     % positive coefficients, so zero is a feasible point
c = -A'*rand(m, 1); % so problem instance is bounded

%********************************************************************
% solve LP to get optimal value
%********************************************************************
cvx_begin
  variable x_min(n)
  minimize (c'*x_min)
  subject to
    A * x_min <= b
cvx_end

f_min = cvx_optval;  % optimal value of LP
fprintf('Optimal value of LP is %0.4f.\n\n', f_min);

%********************************************************************
% primal-dual subgradient method
%********************************************************************
f = []; fconstr = [];
MAX_ITERS = 2500; record_period = 10;

% initial point
x = randn(n, 1);      % need not be feasible
lambda = zeros(m, 1); % must be nonnegative

rho = 1e4;

for k = 1:MAX_ITERS 
  F = pos(A*x - b);
  G = A'*diag(F > 0); %subgradients
  T1 = c + G*(lambda + rho*F);
  T2 = -F;
  alpha = (1/k)/norm([T1;T2]);
  
  % subgradient updates
  x = x - alpha*T1; lambda = lambda - alpha*T2;

  % print out fbest every 100 iterations
  if rem(k, record_period) == 0
      fprintf('iter: %d, f = %.5f, violation = %.5f\n', k, c'*x, max(F));
      % record function value and max violation
      f(end+1) = c'*x;
      fconstr(end+1) = max(F);
  end
end

%********************************************************************
% plot results
%********************************************************************
clf;
semilogy(record_period*(1:length(f)), abs(f-f_min), 'LineWidth', 1.5); hold on;
semilogy(record_period*(1:length(fconstr)), fconstr, 'r-.', 'LineWidth', 1.5); hold off;
set(gca, 'FontSize', 18); xlabel('k');
legend('suboptimality placeholder', 'violation placeholder');
print -depsc linear_program_primal_dual;
