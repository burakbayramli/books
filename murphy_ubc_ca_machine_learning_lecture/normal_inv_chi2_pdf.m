function p = normal_inv_chi2_pdf(mus, sigma2s, mu, kappa, nu, sigma2)
% p(i) = nix2(mus(i), sigma2s(i) | mu, kappa, nu, sigma2)
%      = N(mus(i) | mu, sigma2s(i)/kappa) invchi2(sigma2s(i) | nu, sigma2)

n = length(mus);
p1 = mvnpdf(mus, repmat(mu,n,1), reshape(sigma2s, [1 1 n]) / kappa);
p2 = inv_chi2_pdf(sigma2s, nu, sigma2);
p = p1 .* p2;
