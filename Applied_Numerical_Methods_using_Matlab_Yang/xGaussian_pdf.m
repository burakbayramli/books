function xfx=xGaussian_pdf(x,m,sigma,x0)
xfx=(x-x0).*exp(-(x-m).^2/2/sigma^2)/sqrt(2*pi)/sigma;
