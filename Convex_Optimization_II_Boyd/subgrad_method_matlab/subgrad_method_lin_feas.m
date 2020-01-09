% Solves linear feasibility program
%     find        x
%     subject to  Ax <= b
% using subgradient method with Polyak's optimal step size.
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
%

% generate a problem instance
n = 100; % number of variables
m = 1000; % number of inequalities
% set state so problem is reproducable
randn('state', 0);
rand( 'state', 0);
A = randn(m,n);
b = rand(m,1); % zero is a feasible point
x1 = ones(n,1); % we start the algorithms from an infeasible point

%********************************************************************
% subgradient method with no overstep, eps = 0
%********************************************************************
fconstr = [];

k = 1;
MAX_ITERS = 1500;

% initial point
x = x1;

while k < MAX_ITERS 
  % feasibility check
  [fval,ind] = max(A*x - b);

  if( fval <= 0 ), break, end
  fconstr(end+1) = fval;

  % subgradient and step size computation
  g = A(ind,:)';
  alpha = (fval)/norm(g)^2;

  % subgradient update
  x = x - alpha*g;

  k = k + 1;
  if( rem(k,100) == 0 ), fprintf(1,'iter: %d\n',k), end
end

%********************************************************************
% subgradient with overstep eps = 0.01
%********************************************************************
fconstr1 = [];

k = 1;
EPS = 0.01;

x = x1;

while k < MAX_ITERS 
  % feasibility check
  [fval,ind] = max(A*x - b);

  if( fval <= 0 ), break, end
  fconstr1(end+1) = fval;

  % subgradient and step size computation
  g = A(ind,:)';
  alpha = (fval+EPS)/norm(g)^2;

  % subgradient update
  x = x - alpha*g;

  k = k + 1;
  if( rem(k,100) == 0 ), fprintf(1,'iter: %d\n',k), end
end

%********************************************************************
% subgradient with overstep eps = 0.1
%********************************************************************
fconstr2 = [];

k = 1;
EPS = 0.1;

x = x1;

while k < MAX_ITERS 
  % feasibility check
  [fval,ind] = max(A*x - b);

  if( fval <= 0 ), break, end
  fconstr2(end+1) = fval;

  % subgradient and step size computation
  g = A(ind,:)';
  alpha = (fval+EPS)/norm(g)^2;

  % subgradient update
  x = x - alpha*g;

  k = k + 1;
  if( rem(k,100) == 0 ), fprintf(1,'iter: %d\n',k), end
end

%********************************************************************
% plot results
%********************************************************************
figure(1), clf
set(gca, 'FontSize',18);
semilogy([1:length(fconstr)], fconstr,'r-','LineWidth',1.5), hold on,
semilogy([1:length(fconstr1)],fconstr1,'b--','LineWidth',1.5), hold on,
semilogy([1:length(fconstr2)],fconstr2,'g-.','LineWidth',1), hold off,
xlabel('k');
ylabel('fconstr');
legend('epsil zero','epsil 0.01','epsil 0.1',1);
%print -depsc linear_feas_fconstr
