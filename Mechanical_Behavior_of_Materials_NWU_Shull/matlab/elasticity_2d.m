clear all; close all;
set(0,'defaultlinelinewidth',2)
set(0,'defaultaxesfontsize',16)
set(0,'defaulttextfontsize',16)
set(0,'defaultfigurepapersize',[7,5])
set(0,'defaultfigurepaperposition',[0,0,7,5])
pcylinder=@(r) (1./(3*r)).*(1-r.^-6);
J1=@(r) 2*r.^2+r.^(-4)-3;
pcylinderj=@(Jm,r) pcylinder(r).*exp(J1(r)/Jm);
vol=@(r) r.^3;
rvals=linspace(1,10,200);
plot(rvals,pcylinder(rvals),'b-')
ylabel('P/\kappaR_{0}')
xlabel('R/R_{0}')
print('../membrane_figures/cylinder inflation','-depsc2')

%%
figure
plot(rvals,pcylinder(rvals),'b-')
hold on
plot(rvals,pcylinderj(50,rvals),'r-')
plot(rvals,pcylinderj(20,rvals),'k-')
plot([0,1000],[0.158,0.158],'r--')
ylabel('P/\kappaR_{0}')
xlabel('R/R_{0}')
ylim([0,0.5])
xlim([0,10])
legend('J^{*}=\infty', 'J^{*}=50', 'J^{*}=20','location','best')
print('../membrane_figures/cylinder inflation hardened','-depsc2')

%%
figure
hold on
plot(vol(rvals),pcylinderj(50,rvals),'b-')
plot([0,1000],[0.158,0.158],'r--')
ylabel('P/\kappaR_{0}')
xlabel('V/V_{0}')
ylim([0,0.25])
xlim([0,200])
legend('J^{*}=50','location','best')
print('../membrane_figures/cylinder inflation volume','-depsc2')

