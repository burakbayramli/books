function  p = inverse_gamma_pdf(x, a, b)

% a = shape, b=scale

xdenom = x;
xdenom(find(xdenom==0))=eps;

p = (b^a/gamma(a)) .* x .^-(a+1) .* exp(-b./xdenom);

