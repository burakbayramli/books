%SCM.m
clear all
N=10^3; % Sample size
M=10^2; % Number of trials

g=@(Z) abs(exp(-Z.^2).*cos(3.*pi.*Z)).*...
    exp(-0.5.*((Z-1)./sqrt(2)).^2)./sqrt(2*pi*2);
h=@(Z,mu,sigma) (sigma>0)./(pi.*sigma.*(1+((Z-mu)./sigma).^2));
p=@(Z) 1./(pi.*(1+Z.^2)); % standard cauchy
f=@(x,Z) sum((g(Z)./p(Z)).*log(h(Z,x(1),x(2))));% St. Counterpart

approxnormg=.2330967533;% Approx. norm. const. for g (for plotting)
zz=linspace(-5,5,10^3);% Range to plot densities over
figure,hold on
for k=1:M
    Z=randn(N,1)./randn(N,1);% Z_1,...,Z_N are iid with density p
    sol=fminsearch(@(x) -f(x,Z),[1,2]);% Solve the SC
    plot(zz,h(zz,sol(1),sol(2)),'r-')
end
plot(zz,g(zz)./approxnormg,'k:','LineWidth',2) % Plot g
hold off