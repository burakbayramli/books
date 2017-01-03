function  p = gamma_pdf(x, a, b)

% a = shape, b = rate (inverse scale)
% different parameteriziation to matlab

p = (b^a/gamma(a)) .* x .^(a-1) .* exp(-x.*b);
