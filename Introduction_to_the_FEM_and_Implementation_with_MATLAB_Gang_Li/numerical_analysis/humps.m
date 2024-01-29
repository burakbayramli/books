
clear all;

start_x=0;
end_x=2.0;
n_data_points=7;
n_plotting_points=200;

x=linspace(start_x,end_x,n_data_points);
u0 = 1 ./ ((x-.3).^2 + .01) + 1 ./ ((x-.9).^2 + .04) - 6;

dp(:,1)=x;
dp(:,2)=u0;

x_vector=[start_x:(end_x-start_x)/n_plotting_points:end_x]';
exact=1 ./ ((x_vector-.3).^2 + .01) + 1 ./ ((x_vector-.9).^2 + .04) - 6;

u=compLagIntp1D(dp, x_vector);

hold off;
plot(x_vector,u,'k:','LineWidth',2);
hold on;
plot(x,u0,'k--o','LineWidth',2);
plot(x_vector,exact,'k-','LineWidth',2);
plot(x,u0,'ko','LineWidth',5);

set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
legend('Single 6-th order polynomial',...
      'Piecewise linear polynomial','Actual "humps" function');
print -depsc humps.eps