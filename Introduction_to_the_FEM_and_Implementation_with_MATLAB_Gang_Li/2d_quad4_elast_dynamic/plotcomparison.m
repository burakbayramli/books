clear all;

load Uout.dat;
load Uout2.dat;

plot(Uout*1000, 'k-','LineWidth',2);
hold on;
plot(Uout2*1000, 'k--','LineWidth',2);
axis([0 2200 -1.4 0.21]);
xlabel('Time step');
ylabel('Displacement (mm)');
set(gca,'fontsize',16);
legend('Damped vibration','Undamped vibration');
hold off;
print -depsc dispHistoryComparison.eps

