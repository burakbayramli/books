%This m-file checks the derivations for the skew-normal density
clear;
clc;

delta =[-3 -1 0 5];
sig = 1;
ygrid = linspace(-10,15,5000);
mu = 0;

for j = 1:4;
    subplot(2,2,j);
varian = sig^2 + delta(j)^2;

densa = 2*(1/sqrt(2*pi*varian))*exp(- (1/(2*varian))*( (ygrid-mu).^2));
densb = normcdf( (delta(j)/(sqrt(sig)*sqrt(varian)))*(ygrid-mu) );
density = densa.*densb;
plot(ygrid,density);
end;

