clear all;

load vx1.dat;
load vx2.dat;
load vx3.dat;

figure(2)
clf;
hold on
plot(vx1(:,1),vx1(:,2),'k-o','LineWidth',2);
plot(vx3(:,1),vx3(:,2),'k-+','LineWidth',2);
axis([-0.4 1 0 1]);

xlabel('Veloctiy in x-direction at x=0.5');
ylabel('y-coordinate');
set(gca,'fontsize',16);
legend('4-by-4 mesh','10-by-10 mesh','Location','southeast');
hold off;
%print -depsc vxline.eps
