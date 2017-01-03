mu0 = 0.5; kappa0 = 5; nu0 = 5; sigma20 = 0.5;

%[mus, sigma2s] = meshgrid(-1:0.1:4, 0.1:0.1:4);
[mus, sigma2s] = meshgrid(-1:0.1:1, 0.1:0.1:2);
[nr nc] = size(mus);
p = normal_inv_chi2_pdf(mus(:), sigma2s(:), mu0, kappa0, nu0, sigma20);
p = reshape(p, [nr nc]);

figure(2);clf
surfc(mus, sigma2s, p)
xlabel('\mu')
ylabel('sigma^2')
str=sprintf('NIX(%s=%2.1f, %s=%2.1f, %s=%2.1f, %s=%2.1f)',...
	      '\mu_0', mu0, '\kappa_0', kappa0, '\nu_0', nu0, '\sigma^2_0', sigma20);
title(str)

