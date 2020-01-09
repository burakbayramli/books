% On-line learning and regression example via stochastic subgradients.
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
% 

%********************************************************************
% generate a problem instance
%********************************************************************
n = 10; % number of weights (variables)

% generate a random Sigma_x covariance matrix
randn('state',1); % set state so problem is reproducable
S = randn(n,n);
Sigma_x = S'*S;

% linear relationship
a = randn(n,1)/sqrt(n);
Sigma_y = a'*Sigma_x*a + 0.1;
Sigma_xy = Sigma_x*a;

Sigma = [Sigma_x, Sigma_xy; Sigma_xy', Sigma_y];
R = sqrtm(Sigma);

%********************************************************************
% stochastic subgradient method with diminishing step sizes (sign alg)
%********************************************************************
f = [+Inf]; fbest = [+Inf]; errors = [+Inf];

MAX_ITERS = 5000; 

iter = 1;

w = zeros(n,1); % initial point
whist = [w];

while iter < MAX_ITERS 
  if( rem(iter,500) == 0 ), fprintf(1,'iter: %d\n',iter), end

  % generate a new sample from the distribution
  z = R*randn([n+1 1]); 
  x = z(1:n);  y = z(n+1);

  % noisy subgradient calculation
  e = w'*x - y;
  g = sign(e)*x;

  % step size selection
  alpha = 1/iter;

  % objective values
  errors(end+1) = e;
  fval = abs(e);
  f(end+1) = fval;
  fbest(end+1) = min( fval, fbest(end) );

  % subgradient update
  w = w - alpha*g; iter = iter + 1; whist = [whist, w];
end

w_sign = w;

% compute optimal (lms) weight that minimizes mean-square error
% using analytical formula
w_lms = Sigma(1:n,1:n)\Sigma(1:n,n+1);

%********************************************************************
% report and plot problem results
%********************************************************************
% generate random samples
NSAMPLES = 1000;
Z = R*randn([n+1 NSAMPLES]); 
X = Z(1:n,:);  Y = Z(n+1,:);

err_sign = ( w_sign'*X - Y );
abserr_sign = abs( err_sign );

err_lms    = ( w_lms'*X - Y );
abserr_lms = abs( err_lms );

disp(' ')
fprintf(1,'Sign abs predict error %3.4f\n',mean(abs(err_sign)));
fprintf(1,'Sign mean-square error %3.4f\n',mean((err_sign).^2));
disp(' ')
fprintf(1,'LMS  abs predict error %3.4f\n',mean(abs(err_lms)));
fprintf(1,'LMS  mean-square error %3.4f\n',mean((err_lms).^2));

iters = [1:MAX_ITERS];
figure(1), clf
set(gca, 'FontSize',18);
plot( iters, errors, 'b-','LineWidth', 1 )
axis([1 300 -40 40])
xlabel('k');
ylabel('pred error');
%print -depsc online_reg_errors

% histogram plots
figure(2), clf
edges = linspace(-2,2,40);
[count] = histc( err_sign, edges )';
bar( edges, count, 'histc' );
hline = findobj(gca,'Type','line'); delete(hline)
hpatch = findobj(gca,'Type','patch');
set(hpatch,'FaceColor',[0.91,0.91,0.91])
set(gca, 'FontSize',16);
axis([-2 2 0 150]);
title('sign')
%print -depsc online_reg_dist
