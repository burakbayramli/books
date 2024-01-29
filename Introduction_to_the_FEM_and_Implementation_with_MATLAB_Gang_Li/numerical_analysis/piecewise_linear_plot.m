
x=[0 1 2 3 4 5 6];
u=[2 4 3 8 1 0.5 0.5];


figure(1);
plot(x,u,'b-o','Linewidth',2);
set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
print -depsc piecewise_linear.eps