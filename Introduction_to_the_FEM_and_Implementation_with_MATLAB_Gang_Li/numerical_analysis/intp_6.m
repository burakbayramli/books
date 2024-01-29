
clear all;

x=[0 1 2 3 4 5 6]';
u0=[2 4 3 8 1 0.5 0.5]';

dp(:,1)=x;
dp(:,2)=u0;

x_vector=[0:0.05:6]';

u=compLagIntp1D(dp, x_vector);

hold off;
plot(x_vector,u,'b-','LineWidth',2);
hold on;
plot(x,u0,'b--o','LineWidth',2);
plot(x,u0,'bo','LineWidth',5);
set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
legend('single 6 order polynomial','piecewise linear polynomial');
print -depsc lag_6_example.eps