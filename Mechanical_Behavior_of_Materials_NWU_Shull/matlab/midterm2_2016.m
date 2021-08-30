close all
set(0,'defaultaxesfontsize',16)
set(0,'defaultlinelinewidth',2)
figure
tau=2;
sigma0=1e6;
eps0=0.2;
strainaxes=subplot(1,2,1);
tmin=-2;
plot([tmin 0 0 10], [0 0 eps0 eps0], '-k')
xlabel('t (s)')
ylabel('e')
xlim([tmin, max(t)])
ylim([0 1.2*eps0])
stressaxes=subplot(1,2,2);
t=linspace(0, 10, 100);
plot(t,sigma0*exp(-t./tau),'-k')
hold on
plot([tmin 0 0], [0 0 sigma0],'-k')
ylim([0 1.2*sigma0])
xlim([tmin, max(t)])
xlabel('t (s)')
ylabel('\sigma (Pa)')

set(gcf,'PaperPosition',[0 0 10 5])
set(gcf,'PaperSize',[10 5])
print(gcf, '../figures/Maxwell_stress_strain', '-dsvg')

%%

figure
strain=linspace(0,2,100);
stress=1e7*strain.^0.3;
plot(strain,stress,'k-')
xlabel('engineering strain, e')
ylabel('true stress (Pa)')
set(gcf,'PaperPosition',[0 0 6 5])
set(gcf,'PaperSize',[6 5])
print(gcf, '../figures/true-stress-strain', '-dsvg')



