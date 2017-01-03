% Exampke from Lee p68
% set hyperparams of variance
%  mean = 300, std = 160
[s0,v0]=solve('s0/(v0-2)=300', '2*s0^2/( (v0-2)^2*(v0-4)) = 160^2')
s0 = double(s0); v0 = double(v0);
sigma0_sq = s0/v0;
% set hyperparams of mean
mu0 = 110;
kappa0 = 15;

% data
xs = [141, 102, 73, 171, 137, 91, 81, 157, 146, 69, 121, 134];
N = length(xs);
xbar = mean(xs);
v = std(xs,1).^2

% posterior
vN = v0+N
kappaN = kappa0+N
muN = (kappa0*mu0 + N*xbar)/kappaN
sN = s0 + N*v + (kappa0*N*(xbar-mu0)^2)/kappaN
%sN2 = s0 + N*v + (1/kappa0 + 1/kappaN)^(-1)*(xbar-mu0)^2
sigmaN_sq = sN/vN;

figure(1);clf
mus = 100:0.1:130;
plot(mus, student_t_pdf(mus, mu0, sigma0_sq/kappa0, v0), 'b-', 'linewidth',2);
hold on
plot(mus, student_t_pdf(mus, muN, sigmaN_sq/kappaN, vN), 'r:','linewidth',2);
legend('prior', 'posterior')
title('p(mean)=St()')

figure(2);clf
vars = 1:5:1000;
plot(vars, inv_chi2_pdf(vars, v0, sigma0_sq),  'b-', 'linewidth', 2);
hold on
plot(vars, inv_chi2_pdf(vars, vN, sigmaN_sq),  'r:', 'linewidth', 2);
legend('prior', 'posterior')
title('p(sigma^2)=\chi^{-2}()')

figure(3);clf
xs = inv_chi2_rnd(v0, sigma0_sq, 1, 1000);
[f, xi] = ksdensity(xs);
plot(xi, f,  'b-', 'linewidth', 2);
hold on
xs = inv_chi2_rnd(vN, sigmaN_sq, 1, 1000);
[f, xi] = ksdensity(xs);
plot(xi, f,  'r:', 'linewidth', 2);
