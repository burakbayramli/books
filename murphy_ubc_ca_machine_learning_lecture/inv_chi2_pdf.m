function p = inv_chi2_pdf(xs, nu, sigma2)

p = inverse_gamma_pdf(xs, nu/2, (nu*sigma2)/2); 

if 0
  % this gives the same results
tmp = xs .^ (-nu/2-1) .* exp(-nu*sigma2 ./ (2*xs) );
q = 1/gamma(nu/2) * (nu*sigma2/2)^(nu/2) .* tmp;
%p=q;
assert(approxeq(p,q))
end
