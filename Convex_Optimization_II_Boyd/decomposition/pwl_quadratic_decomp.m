% 5 subsystems with quadratic plus piecewise linear objectives
% and with 4 coupling constraints

%
% generate a problem instance
%
n = 10; % number of private variables per subsystem
m = 20; % number of affine terms in the piecewise linear fct
p = 9;   % total number of public variables
p1 = 1; p2 = 2; p3 = 1; p4 = 3; p5 = 2; % public vars per subsystem
% netlist matrix
E = [ 1     0     0     0
      1     0     0     0
      0     1     0     0
      0     0     1     0
      0     0     1     0
      1     0     0     0
      0     0     0     1
      0     0     0     1
      0     1     0     0];
% piecewise linear objective data
randn('state',1); % set state so problem is reproducable
A1 = randn(m,n+p1); b1  = randn(m,1);
A2 = randn(m,n+p2); b2  = randn(m,1);
A3 = randn(m,n+p3); b3  = randn(m,1);
A4 = randn(m,n+p4); b4  = randn(m,1);
A5 = randn(m,n+p5); b5  = randn(m,1);
% offsets for quadratic fcts
c1 = sqrt(2)*randn(n+p1,1);
c2 = sqrt(2)*randn(n+p2,1);
c3 = sqrt(2)*randn(n+p3,1);
c4 = sqrt(2)*randn(n+p4,1);
c5 = sqrt(2)*randn(n+p5,1);
mu = 0.1; % weight for the quadratics

%
% solve centralized problem using CVX
%
cvx_begin
  variables x1(n) x2(n) x3(n) x4(n) x5(n) % private variables
  variables y1 y2 y3 y4 y5 y6 y7 y8 y9    % public variables 
  % objective function
  minimize( ...
     max(A1*[x1;y1]+b1)       + mu*sum_square( [x1;y1] - c1 )       + ... 
     max(A2*[x2;y2;y3]+b2)    + mu*sum_square( [x2;y2;y3] - c2 )    + ...
     max(A3*[x3;y4]+b3)       + mu*sum_square( [x3;y4] - c3 )       + ...
     max(A4*[x4;y5;y6;y7]+b4) + mu*sum_square( [x4;y5;y6;y7] - c4 ) + ...
     max(A5*[x5;y8;y9]+b5)    + mu*sum_square( [x5;y8;y9] - c5 ) ); 
  % subsystem relations
  subject to
    y1 == y2;
    y1 == y6;
    y3 == y9;
    y4 == y5;
    y7 == y8;
cvx_end
f_opt = cvx_optval;

%
% distributed solution using dual decomposition
%
cvx_quiet(true);

MAX_ITER = 20;
alpha = .5;    % fixed step size

nu = zeros(p,1); % initial dual point (its average is zero)
flower = []; fupper = []; bupper = []; residual = [];

for iter = 1:MAX_ITER

  disp(iter)

  % subsystems optimize (separately)
  % subsystem 1
  cvx_begin
    variables x1(n) y1
    minimize( max(A1*[x1;y1]+b1) + mu*sum_square( [x1;y1] - c1 ) + ...
              nu(1)*y1 )
  cvx_end
  q1 = cvx_optval;
  % subsystem 2
  cvx_begin
    variables x2(n) y2 y3
    minimize( max(A2*[x2;y2;y3]+b2) + mu*sum_square( [x2;y2;y3] - c2 ) + ...
              nu(2)*y2 + nu(3)*y3 )
  cvx_end
  q2 = cvx_optval;
  % subsystem 3
  cvx_begin
    variables x3(n) y4
    minimize( max(A3*[x3;y4]+b3) + mu*sum_square( [x3;y4] - c3 ) + ...
              nu(4)*y4 )
  cvx_end
  q3 = cvx_optval;
  % subsystem 4
  cvx_begin
    variables x4(n) y5 y6 y7
    minimize( max(A4*[x4;y5;y6;y7]+b4) + mu*sum_square( [x4;y5;y6;y7] - c4 ) + ...
              nu(5)*y5 + nu(6)*y6 + nu(7)*y7 )
  cvx_end
  q4 = cvx_optval;
  % subsystem 5
  cvx_begin
    variables x5(n) y8 y9
    minimize( max(A5*[x5;y8;y9]+b5) + mu*sum_square( [x5;y8;y9] - c5 ) + ...
              nu(8)*y8 + nu(9)*y9 )
  cvx_end
  q5 = cvx_optval;

  % construct vector of public variables
  y = [y1; y2; y3; y4; y5; y6; y7; y8; y9];
  % compute average value of public variables over each net
  zhat = E\y;
  % update prices on public variables
  nu = nu + alpha*(y-E*zhat);
  % compute lower bound on the objective
  flower(end+1) = q1 + q2 + q3 + q4 + q5;
  % consistency constraint residual
  residual(end+1) = norm( y-E*zhat );

  % compute worse upper bound using zhat
  yhat = E*zhat;
  bupper(end+1) = ...
    max(A1*[x1;yhat(1)]+b1) + mu*sum_square( [x1;yhat(1)] - c1 ) + ...
    max(A2*[x2;yhat(2);yhat(3)]+b2) + ...
              mu*sum_square( [x2;yhat(2);yhat(3)] - c2 ) + ...
    max(A3*[x3;yhat(4)]+b3) + mu*sum_square( [x3;yhat(4)] - c3 ) + ...
    max(A4*[x4;yhat(5);yhat(6);yhat(7)]+b4) + ...
              mu*sum_square( [x4;yhat(5);yhat(6);yhat(7)] - c4 ) + ...
    max(A5*[x5;yhat(8);yhat(9)]+b5) + ...
              mu*sum_square( [x5;yhat(8);yhat(9)] - c5 );

  % compute better upper bound by re-solving primal problems with zhat
  % subsystem 1
  cvx_begin
    variables x1(n)
    minimize( max(A1*[x1;yhat(1)]+b1) + mu*sum_square( [x1;yhat(1)] - c1 ) )
  cvx_end
  f1 = cvx_optval;
  % subsystem 2
  cvx_begin
    variables x2(n)
    minimize( max(A2*[x2;yhat(2);yhat(3)]+b2) + ...
              mu*sum_square( [x2;yhat(2);yhat(3)] - c2 ) ) 
  cvx_end
  f2 = cvx_optval;
  % subsystem 3
  cvx_begin
    variables x3(n)
    minimize( max(A3*[x3;yhat(4)]+b3) + mu*sum_square( [x3;yhat(4)] - c3 ) )
  cvx_end
  f3 = cvx_optval;
  % subsystem 4
  cvx_begin
    variables x4(n)
    minimize( max(A4*[x4;yhat(5);yhat(6);yhat(7)]+b4) + ...
              mu*sum_square( [x4;yhat(5);yhat(6);yhat(7)] - c4 ) ) 
  cvx_end
  f4 = cvx_optval;
  % subsystem 5
  cvx_begin
    variables x5(n)
    minimize( max(A5*[x5;yhat(8);yhat(9)]+b5) + ...
              mu*sum_square( [x5;yhat(8);yhat(9)] - c5 ) )
  cvx_end
  f5 = cvx_optval;

  % upper bound
  fupper(end+1) = f1 + f2 + f3 + f4 + f5;
end
cvx_quiet(false);

iters = [1:12];
figure(1), clf
set(gca,'FontSize',18);
plot(iters,flower(iters),'b-',iters,fupper(iters),'k--', ...
     iters,bupper(iters),'r-.','LineWidth',1.5);
xlabel('k'), legend('flower b','fupper b','upper b')
print -depsc pwl_quadratic_bounds

figure(2), clf
set(gca,'FontSize',18);
semilogy(iters,residual(iters),'LineWidth',1.5);
xlabel('k'), ylabel('residual')
print -depsc pwl_quadratic_residual
