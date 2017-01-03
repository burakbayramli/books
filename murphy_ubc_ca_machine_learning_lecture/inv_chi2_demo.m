figure(1);clf;
sigma2s  = 0.01:0.01:2;

nu0 = [1 50 1 50]; sigma20 = [1 1 0.5 0.5];

for i=1:4
  p = inv_chi2_pdf(sigma2s(:), nu0(i), sigma20(i));
  subplot(2,2,i)
  plot(sigma2s, p, 'linewidth', 2)
  str=sprintf('%s(%s=%2.1f, %s=%2.1f)',...
	      '\chi^{-2}', '\nu_0', nu0(i), '\sigma^2_0', sigma20(i));
  title(str)
end
