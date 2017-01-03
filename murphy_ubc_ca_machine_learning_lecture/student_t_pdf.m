function p = student_t_pdf(xs, mu, sigma2, nu)
% mu = mean, sigma2 = variance, nu = dof

tmp = ((xs-mu).^2) ./  (nu*sigma2);
p = gamma(nu/2+0.5)/gamma(nu/2) * (sigma2*pi*nu)^(-0.5) * (1+tmp).^(-(nu+1)/2);

%p2 = tpdf( (xs-mu) ./ sqrt(sigma2), nu);
%assert(approxeq(p,p2))
