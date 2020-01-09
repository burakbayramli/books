% Solve linear program
%   minimize    c'*x
%   subject to  Ax <= b
% using subgradient method
%
% generates plot of best objective value versus iterations
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
%

% generate a problem instance
n = 20; % number of variables
m = 200; % number of inequalities
% set state so problem is reproducible
randn('state', 0);
rand( 'state', 0);
A = randn(m,n);
b = rand(m,1);     % positive coefficients, so zero is a feasible point
c = -A'*rand(m,1); % so problem instance is bounded

%********************************************************************
% solve LP to get optimal value
%********************************************************************
cvx_begin
  variable x_min(n)
  minimize ( c'*x_min )
  subject to
    A*x_min <= b
cvx_end

f_min = cvx_optval;  % optimal value of LP
fprintf(1,'Optimal value of LP is %0.4f.\n\n',f_min);

%********************************************************************
% subgradient method
%********************************************************************
f = [+Inf]; fbest = [+Inf]; fconstr = [];

k = 1;
MAX_ITERS = 2500;
EPS = 1e-3; % we'll overstep this distance in feasibility steps

% initial point
x = zeros(n,1);

while k < MAX_ITERS 
  % feasibility check
  [fval,ind] = max(A*x - b);

  % subgradient and step size computation
  if( fval > 0 ) % feasibility step
    fbest(end+1) = fbest(end);
    g = A(ind,:)';
    alpha = (fval+EPS)/norm(g)^2;

  else % optimality step
    f(end+1) = c'*x;
    fbest(end+1) = min( c'*x, fbest(end) );
    g = c;
    alpha = 1/k;
  end

  % constraint violation values
  fconstr(end+1) = fval;

  % subgradient update
  x = x - alpha*g; k = k + 1;

  % print out fbest every 100 iterations
  if( rem(k,100) == 0 ), fprintf(1,'iter: %d\n',k), end
end

%********************************************************************
% plot results
%********************************************************************
%figure(1), clf
set(gca, 'FontSize',18);
semilogy( [1:length(fbest)], fbest-f_min,'LineWidth',1.5 )
xlabel('k');
ylabel('fbest - fmin');
%% print -depsc linear_program_fbest
