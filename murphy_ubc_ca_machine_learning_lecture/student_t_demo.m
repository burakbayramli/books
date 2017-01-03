% student_t_demo.m

figure(1);clf;
xs = -8:0.01:8;

mu0 = [0 0 0 0]; sigma20 = [1 1 1 3];
nu0 = [1 3 10 1];

for i=1:4
  p = student_t_pdf(xs(:), mu0(i), sigma20(i), nu0(i));
  subplot(2,2,i)
  plot(xs, p, 'linewidth', 2)
  str=sprintf('T(%s=%2.1f, %s=%2.1f, %s=%2.1f)',...
	      '\mu_0', mu0(i), '\nu_0', nu0(i), '\sigma^2_0', sigma20(i));
  title(str)

  hold on
  p2 = normpdf(xs(:), mu0(i), sqrt(sigma20(i)));
  subplot(2,2,i)
  plot(xs, p2, 'r:', 'linewidth', 2)
end
