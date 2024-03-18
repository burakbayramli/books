% test demoflow RK scheme
close all
clear all
format compact
%
coe = [0.2000 0.2500 0.3333 0.5000 1]; % fifth order
coe = [0.0533 0.1263 0.2375 0.4414 1]; % approx second
coe = [0.0695 0.1602 0.2898 0.5060 1]; % better second
n = length(coe);
ns = 500;
x = linspace(0,1,ns);
q0 = ones(size(x));
q = q0;
c = ones(1,n+1);
for k = 1:n
    c(k+1) = coe(n+1-k)*c(k);
    q = q0+coe(k)*x.*q;
end
c
plot(x,q)
hold on
plot(x,exp(x),'.-r')
er = exp(x)-q;
er(2:end)./x(2:end).^3;
figure
loglog(x(2:end),abs(er(2:end)))
hold on
loglog([0.001,1],[1e-8,1e-2],'--')