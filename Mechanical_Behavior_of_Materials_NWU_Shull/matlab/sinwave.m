close all
set(0,'defaultlinelinewidth',2)
set(0,'defaultaxesfontsize',16)
set(0,'defaultfigurepaperposition',[0,0,7,5])
set(0,'defaultfigurepapersize',[7,5])
xdata=linspace(0,6*pi,300);
subplot(2,1,1)
hold on
plots(1)=plot(xdata,sin(xdata),'-r');
plots(2)=plot(xdata,sin(xdata+pi/2),'-b');
legend(plots(1:2),'strain','stress')
title('\delta=90^{\circ}')
ylabel('strain, stress')
xlabel('time')
set(gca,'XTickLabel',[])
set(gca,'xtick',[])
set(gca,'ytick',[])

subplot(2,1,2)
hold on
plots(3)=plot(xdata,sin(xdata),'-r');
plots(4)=plot(xdata,sin(xdata+pi/4),'-b');
legend(plots(3:4),'strain','stress')
title('\delta=45^{\circ}')
xlabel('time')
ylabel('strain, stress')
set(gca,'XTickLabel',[])
set(gca,'xtick',[])
set(gca,'ytick',[])

print(gcf,'../figures/sinewave.eps', '-depsc2')