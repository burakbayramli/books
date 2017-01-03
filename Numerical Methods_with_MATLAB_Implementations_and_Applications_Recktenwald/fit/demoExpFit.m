function demoExpFit
% demoExpFit  Fit exponentially decay of capacitor voltage
%
% Synopsis:  demoExpFit
%
% Input:  none
%
% Output:  Fit coefficients for v = c(1)*exp(c(2)*t) and plot of
%          original data and fit function

load capacitor.dat;   t = capacitor(:,1);  v = capacitor(:,2);

ct = linefit(t,log(v));     %  Line fit to transformed data
c = [exp(ct(2));  ct(1)]    %  Extract parameters from transformation

tfit = linspace(min(t),max(t));
vfit = c(1)*exp(c(2)*tfit);
plot(v,t,'o',vfit,tfit,'-')
xlabel('time');   ylabel('capacitor voltage')
