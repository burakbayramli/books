clear all; close all
e1=2e5;
eta3=5e6;
e2=2e5;
eta2=5e6;
tau2=eta2/e2;
sigma=1e5;
t=linspace(0,100, 200);
strain=@(t) sigma/e1+(sigma/e2)*(1-exp(-t/tau2))+sigma*t/eta3;
plot([0 t],[0 strain(t)],'linewidth',2)
set(gca,'fontsize',16)
xlim([-10,100])
xlabel('t (s)', 'fontsize',16)
ylabel('e', 'fontsize',16)
set(gcf,'paperposition',[0 0 7 5], 'papersize', [7 5])
print(gcf,'../figures/creepdata.svg', '-dsvg')