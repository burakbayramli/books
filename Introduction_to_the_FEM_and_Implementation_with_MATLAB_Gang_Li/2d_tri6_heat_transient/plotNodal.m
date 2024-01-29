

load T455.dat;
load T560.dat;
load T990.dat;

figure(1);
clf;
hold off;

plot(T455,'k-','LineWidth',2);
hold on;
plot(T560,'k--','LineWidth',2);
plot(T990,'k:','LineWidth',2);


set(gca,'fontsize',16);
xlabel('Time step (\Deltat=10^{-3} s)');
ylabel('Temperature (^oC)');


hold off
set(gca,'fontsize',16);
legend('node 455 (0.1760, 0.0649)',...
  'node 560 (0.0308, 0.0795) ', ...
  'node 990 (0.0865, 0.0843)','Location','southeast');

axis([0 500 -5 60]);

print -depsc Tcurves.eps
