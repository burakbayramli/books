% Solves piecewise linear minimization problem
%     minimize   max_i=1..m (a_i'x + b_i)
% using subgradient method with errors in subgradient evaluations.
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
%

% generate a problem instance
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
% subgradient method with errors
%********************************************************************
MAX_ITERS = 5000; 
sigmasqr = 0.25; % covariance of the noisy perturbation is scaled identity

% run error-free realization and two realizations with errors 
[x,histo] = sgm_pwl_error_sqrsum(A,b,x_1,1,0,MAX_ITERS); 
[x,hist1] = sgm_pwl_error_sqrsum(A,b,x_1,1,sigmasqr,MAX_ITERS); 
[x,hist2] = sgm_pwl_error_sqrsum(A,b,x_1,1,sigmasqr,MAX_ITERS); 

% setup plot data
iters = [1:MAX_ITERS];
fo = histo{1}; fbesto = histo{2};
f1 = hist1{1}; fbest1 = hist1{2};
f2 = hist2{1}; fbest2 = hist2{2};

%********************************************************************
% generate average convergence curves
%********************************************************************
NSAMPLES = 100;
fbest_hist = zeros(NSAMPLES,MAX_ITERS);

for snum = 1:NSAMPLES
  [x,hist] = sgm_pwl_error_sqrsum(A,b,x_1,1,sigmasqr,MAX_ITERS);
  fbest_hist(snum,:) = hist{2};
  fprintf(1,'sample number = %d\n',snum);
end

%********************************************************************
% plots
%********************************************************************
figure(1), clf
set(gca, 'FontSize',18);
semilogy( iters, fbesto-f_min, 'k:','LineWidth',1.5 ); hold on;
semilogy( iters, fbest1-f_min, 'b--','LineWidth',1.5 );
semilogy( iters, fbest2-f_min, 'r-.','LineWidth',1.5 );
hold off
axis([1 5000 4e-4 2])
xlabel('k');
ylabel('fbest - fmin');
legend('noise-free realize','realize1','realize2',1);
% print -depsc pwl_error_fbest_realize

figure(2), clf
iters = [1:MAX_ITERS];
set(gca, 'FontSize',18);
avg = mean(fbest_hist-f_min);
err = std(fbest_hist-f_min);
semilogy(iters, fbesto-f_min, 'k:','LineWidth',1 ); hold on;
semilogy(iters, sum(fbest_hist-f_min)/NSAMPLES, 'b');
for i = [250:250:MAX_ITERS]
  semilogy(i*[1;1], avg(i)+[err(i);-err(i)], 'r','LineWidth',2);
  semilogy(i, avg(i), 'ro');
end;
hold off
axis([1 5000 4e-4 2])
xlabel('k');
ylabel('average fbest - fmin');
% print -depsc pwl_error_fbest_average

figure(3), clf
% histogram plots
edges = logspace(-4,0,40);
[count_1] = histc( fbest_hist(:,250)-f_min, edges )';
[count_2] = histc( fbest_hist(:,1000)-f_min, edges )';
[count_3] = histc( fbest_hist(:,5000)-f_min, edges )';
common_axis = [5e-4 1 0 30];

subplot(3,1,1), bar( edges, count_1, 'histc' ); hold on,
hline = findobj(gca,'Type','line'); delete(hline)
hpatch = findobj(gca,'Type','patch');
set(hpatch,'FaceColor',[0.91,0.91,0.91]), hold off,
set(gca,'Xscale','log','FontSize',16), axis([common_axis])
title('iter 250')

subplot(3,1,2), bar( edges, count_2, 'histc' ), hold on,
h = findobj(gca,'Type','line'); delete(h)
hpatch = findobj(gca,'Type','patch');
set(hpatch,'FaceColor',[0.91,0.91,0.91]), hold off,
set(gca,'Xscale','log','FontSize',16), axis([common_axis])
title('iter 1000')

subplot(3,1,3), bar( edges, count_3, 'histc' ), hold on,
h = findobj(gca,'Type','line'); delete(h)
hpatch = findobj(gca,'Type','patch');
set(hpatch,'FaceColor',[0.91,0.91,0.91]), hold off,
set(gca,'Xscale','log','FontSize',16), axis([common_axis])
title('iter 5000')
% print -depsc pwl_error_fbest_dist
