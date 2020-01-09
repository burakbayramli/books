% Solves expected piecewise linear minimization problem
%      minimize   E( max_i=1..m (a_i'x + b_i) )
% using stochastic subgradient method.
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
% 
fprintf(1,'This script takes a few minutes to run...\n');

% generate a problem instance
n = 20; % number of variables
m = 100; % number of terms
randn('state',1); % set state so problem is reproducable
Abar = randn(m,n);
bbar = randn(m,1);

%********************************************************************
% compute near optimal point by running the algorithm for a long time
% we save the optimal value to a MAT file DATA_AVGPWL_OPTVAL, so that
% this long computation does not have to be repeated every time
%********************************************************************
if ~exist( 'DATA_AVGPWL_OPTVAL.mat','file' )
  MAX_ITERS = 5000;
  x_1 = zeros(n,1);
  [xo,histo] = stoch_subg_exp_pwl_sqrsum(Abar,bbar,1000,x_1,MAX_ITERS);
  fo = histo{1}; fbesto = histo{2}; xso = histo{3};
  f_min = min( fbesto );

  save DATA_AVGPWL_OPTVAL xo histo f_min
else
  load DATA_AVGPWL_OPTVAL
end

%********************************************************************
% lower bound solution: minimize max_i E(a_i'x + b_i)
%********************************************************************
% LP solution
cvx_begin
  variable x_nom(n)
  minimize( max(Abar*x_nom + bbar) )
cvx_end

f_nom = cvx_optval;
fprintf(1,'Nominal performance of x_nom is %3.4f\n', f_nom);

%********************************************************************
% heuristic solution: minimize max_i (a_i'x + b_i + lambda ||x||)
%********************************************************************
lambda = 1;

% SOCP solution
cvx_begin
  variable x_heur(n)
  minimize( max(Abar*x_heur + bbar + lambda*norm(x_heur)) )
cvx_end

f_heur = cvx_optval;
fprintf(1,'Nominal performance of x_heur is %3.4f\n', max(Abar*x_heur+bbar));

%********************************************************************
% subgradient method
%********************************************************************
MAX_ITERS = 5000; 

% initial point
x_1 = zeros(n,1);

% run subgradient method with constant step length for different gammas
[x,hist1] = stoch_subg_exp_pwl_sqrsum(Abar,bbar,1,x_1,MAX_ITERS);
[x,hist2] = stoch_subg_exp_pwl_sqrsum(Abar,bbar,10,x_1,MAX_ITERS); 
[x,hist3] = stoch_subg_exp_pwl_sqrsum(Abar,bbar,100,x_1,MAX_ITERS); 

% setup plot data
iters = [1:MAX_ITERS];
fbest_nom1 = hist1{1}; xs1 = hist1{3};
fbest_nom2 = hist2{1}; xs2 = hist2{3};
fbest_nom3 = hist3{1}; xs3 = hist3{3};
fbest_nomo = histo{1}; xso = histo{3};

% compute the nominal performance
fprintf(1,'Best nom perf when K = 1   is %3.4f\n', min(fbest_nom1));
fprintf(1,'Best nom perf when K = 10  is %3.4f\n', min(fbest_nom2));
fprintf(1,'Best nom perf when K = 100 is %3.4f\n', min(fbest_nom3));

%********************************************************************
% generate convergence curves
%********************************************************************
NSAMPLES = 1000;
iters = [1,100:100:MAX_ITERS];

sigma_A = sqrt(5);
sigma_b = sqrt(5);

fbest1_mc = []; fbest2_mc = []; fbest3_mc = []; fbesto_mc = [];

for iter = [iters]
  if( rem(iter,500) == 0 ), fprintf(1,'iter: %d\n',iter), end
  % generate random samples
  ADelta = sigma_A/sqrt(m)*randn([m*NSAMPLES n]);
  bDelta = sigma_b/sqrt(m)*randn([m NSAMPLES]);

  Rbar = Abar*xs1(:,iter) + bbar;
  F = Rbar*ones(1,NSAMPLES) + reshape(ADelta*xs1(:,iter),m,NSAMPLES) + bDelta;
  fbest1_mc(end+1) = 1/NSAMPLES*( sum( max(F) ) );

  Rbar = Abar*xs2(:,iter) + bbar;
  F = Rbar*ones(1,NSAMPLES) + reshape(ADelta*xs2(:,iter),m,NSAMPLES) + bDelta;
  fbest2_mc(end+1) = 1/NSAMPLES*( sum( max(F) ) );

  Rbar = Abar*xs3(:,iter) + bbar;
  F = Rbar*ones(1,NSAMPLES) + reshape(ADelta*xs3(:,iter),m,NSAMPLES) + bDelta;
  fbest3_mc(end+1) = 1/NSAMPLES*( sum( max(F) ) );

  Rbar = Abar*xso(:,iter) + bbar;
  F = Rbar*ones(1,NSAMPLES) + reshape(ADelta*xso(:,iter),m,NSAMPLES) + bDelta;
  fbesto_mc(end+1) = 1/NSAMPLES*( sum( max(F) ) );
end

f_min = min( [fbest1_mc, fbest2_mc, fbest3_mc, fbesto_mc] );

% create pdf of the best subgrad K = 100 and nominal problem
fbest3 = []; NSAMPLES = 100;
for iter = [1:10:MAX_ITERS]
  if( rem(iter,500) == 0 ), fprintf(1,'iter: %d\n',iter), end
  ADelta = sigma_A/sqrt(m)*randn([m*NSAMPLES n]);
  bDelta = sigma_b/sqrt(m)*randn([m NSAMPLES]);

  Rbar = Abar*xs3(:,iter) + bbar;
  F = Rbar*ones(1,NSAMPLES) + reshape(ADelta*xs3(:,iter),m,NSAMPLES) + bDelta;
  fbest3(end+1) = 1/NSAMPLES*( sum( max(F) ) );
end
[fval,findex] = find( fbest3 == min(fbest3) );
xk100 = xs3(:,findex(end));

% generate random samples
ADelta = sigma_A/sqrt(m)*randn([m*NSAMPLES n]);
bDelta = sigma_b/sqrt(m)*randn([m NSAMPLES]);

Rbar = Abar*xk100 + bbar;
F = Rbar*ones(1,NSAMPLES) + reshape(ADelta*xk100,m,NSAMPLES) + bDelta;
fbest_k100_max = max(F);

Rbar = Abar*x_nom + bbar;
F = Rbar*ones(1,NSAMPLES) + reshape(ADelta*x_nom,m,NSAMPLES) + bDelta;
fbest_nom_max = max(F);

Rbar = Abar*x_heur + bbar;
F = Rbar*ones(1,NSAMPLES) + reshape(ADelta*x_heur,m,NSAMPLES) + bDelta;
fbest_heur_max = max(F);

%********************************************************************
% plots
%********************************************************************
fbest1p(1) = fbest1_mc(1);
fbest2p(1) = fbest2_mc(1);
fbest3p(1) = fbest3_mc(1);

for i = 2:length(fbest1_mc)
  if(fbest1p(i-1) > fbest1_mc(i)), fbest1p(i) = fbest1_mc(i);
  else fbest1p(i) = fbest1p(i-1); end
  if(fbest2p(i-1) > fbest2_mc(i)), fbest2p(i) = fbest2_mc(i);
  else fbest2p(i) = fbest2p(i-1); end
  if(fbest3p(i-1) > fbest3_mc(i)), fbest3p(i) = fbest3_mc(i);
  else fbest3p(i) = fbest3p(i-1); end
end

figure(1), clf
set(gca, 'FontSize',18);
semilogy( iters, fbest1p-f_min, 'b--','LineWidth',1.5 ), hold on,
semilogy( iters, fbest2p-f_min, 'k:','LineWidth',1.5 );
semilogy( iters, fbest3p-f_min, 'r-.','LineWidth',1.5 );
hold off
axis([1 2000 1e-3 2])
xlabel('k');
ylabel('fbest - fmin');
legend('K = 1','K = 10','K = 100 label',1);
%print -depsc expected_avgpwl_fbest

figure(2), clf
% histogram plots
edges = linspace(1,3,40);
[count_1] = histc( fbest_nom_max, edges )';
[count_2] = histc( fbest_k100_max, edges )';
[count_3] = histc( fbest_heur_max, edges )';
common_axis = [1 3 0 22];

% nominal distribution
subplot(3,1,1), bar( edges, count_1, 'histc' ); hold on,
hline = findobj(gca,'Type','line'); delete(hline)
hpatch = findobj(gca,'Type','patch');
set(hpatch,'FaceColor',[0.91,0.91,0.91])
set(gca, 'FontSize',16);
plot([f_nom f_nom],[0 200],'k:','LineWidth',1.5)
text(f_nom+.01,19,'nom nom');
plot(mean(fbest_nom_max)*[1;1],[0 200],'k-')
text(mean(fbest_nom_max)+.01,19,'nom mean');
hold off,
axis([common_axis])
title('nominal')

% heuristic solution distribution
subplot(3,1,2), bar( edges, count_3, 'histc' ); hold on,
hline = findobj(gca,'Type','line'); delete(hline)
hpatch = findobj(gca,'Type','patch');
set(hpatch,'FaceColor',[0.91,0.91,0.91])
set(gca, 'FontSize',16);
plot(max(Abar*x_heur+bbar)*[1;1],[0 200],'k:','LineWidth',1.5)
text(max(Abar*x_heur+bbar)+.01,19,'heur nom');
plot(mean(fbest_heur_max)*[1;1],[0 200],'k-')
text(mean(fbest_heur_max)+.01,19,'heur mean');
hold off,
axis([common_axis])
title('heuristic')

% best K = 100 distribution
subplot(3,1,3), bar( edges, count_2, 'histc' ), hold on,
h = findobj(gca,'Type','line'); delete(h)
hpatch = findobj(gca,'Type','patch');
set(hpatch,'FaceColor',[0.91,0.91,0.91])
set(gca, 'FontSize',16);
plot(max(Abar*xk100+bbar)*[1;1],[0 200],'k:','LineWidth',1.5),
text(max(Abar*xk100+bbar)+.01,19,'k100 nom');
plot(mean(fbest_k100_max)*[1;1],[0 200],'k-')
text(mean(fbest_k100_max)+.01,19,'k100 mean');
hold off,
axis([common_axis])
title('K = 100')
% print -depsc expected_pwl_fbest_dist
