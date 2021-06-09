function y = frayritz10_8load(x)
global xi;
y=(1-501*abs(xi-x)).*exp(10*x).*cos(12*x);