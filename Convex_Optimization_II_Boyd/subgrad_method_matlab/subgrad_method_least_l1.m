% Solves least l1-norm minimization problem
%     minimize    ||x||_1
%     subject to  Ax = b
% using projected subgradient method
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
%

% generate a problem instance
n = 1000; % number of variables
m = 50; % number of equality constraints
randn('state',1); % set state so problem is reproducable
A = randn(m,n);
b = randn(m,1);

% threshold value below which we consider an element to be zero
DELTA = 1e-8;

%********************************************************************
% compute an optimal point by solving an LP (use CVX)
%********************************************************************
cvx_begin
  variable x_min(n)
  minimize( norm( x_min, 1 ) )
  subject to
    A*x_min == b;
cvx_end

f_min = cvx_optval;
fprintf(1,'Optimal value is %0.4f.\n',f_min);
nnz = length(find( abs(x_min) > DELTA ));
fprintf(1,'Found a feasible x in R^%d that has %d nonzeros.\n\n',n,nnz);

% initial point needs to satisfy A*x1 = b (can use least-norm solution)
x1 = pinv(A)*b;

%********************************************************************
% subgradient method computation
%********************************************************************
f = [+Inf]; fbest = [+Inf];

k = 1;
x = x1;

MAX_ITERS = 3000;

while k < MAX_ITERS 

  % subgradient calculation
  fval = norm(x, 1);
  g = (x > DELTA) - (x < -DELTA); % sign(x) with DELTA tolerance

  % step size selection
  alpha = 0.1/k;

  % keep objective values
  f(end+1) = fval;
  fbest(end+1) = min( fval, fbest(end) );

  % subgradient update
  x = x - alpha*(g - A'*(A'\g));  k = k + 1;

  if( rem(k,500) == 0 ), fprintf(1,'iter: %d\n',k), end
end

%********************************************************************
% plot results
%********************************************************************
figure(1), clf
set(gca, 'FontSize',18);
semilogy( [1:MAX_ITERS], fbest-f_min,'LineWidth',1.5 )
xlabel('k');
ylabel('fbest - fmin');
%print -depsc least_l1_norm_fbest
