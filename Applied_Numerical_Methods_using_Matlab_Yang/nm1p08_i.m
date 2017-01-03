%nm1p08: to try using quad() with in-line functions and m-file functions
clear
m=1; sigma=2;
int_xGausspdf=quad('xGaussian_pdf',m-10,m+10,[],0,m,sigma,1)
Gpdf='exp(-(x-m).^2/2/sigma^2)/sqrt(2*pi)/sigma';
xGpdf=inline(['(x-x0).*' Gpdf],'x','m','sigma','x0');
int_xGpdf=quad(xGpdf,m-10,m+10,[],0,m,sigma,1)
