
clear all;

start_x=0;
end_x=2.0;
n_data_points=7;
n_plotting_points=200;

x=linspace(start_x,end_x,n_data_points);
u0 = 1 ./ ((x-.3).^2 + .01) + 1 ./ ((x-.9).^2 + .04) - 6;
x1=x;
u1=u0;

x_vector=[start_x:(end_x-start_x)/n_plotting_points:end_x]';
exact=1 ./ ((x_vector-.3).^2 + .01) + 1 ./ ((x_vector-.9).^2 + .04) - 6;

hold off;
plot(x_vector,exact,'k-','LineWidth',2);
hold on;
plot(x,u0,'k--','LineWidth',2);

nc=(n_data_points-1)/2;

for i=1:nc
  start_x=(i-1)*2/nc
  end_x=(i)*2/nc
  n_data_points=3;
  n_plotting_points=50;

  x=linspace(start_x,end_x,n_data_points);
  u0 = 1 ./ ((x-.3).^2 + .01) + 1 ./ ((x-.9).^2 + .04) - 6;

  x_vector=[start_x:(end_x-start_x)/n_plotting_points:end_x]';

  clear dp;
  dp(:,1)=x;
  dp(:,2)=u0;

  u=CompLagIntp1D(dp, x_vector);

  plot(x_vector,u,'k:','LineWidth',2);
end

plot(x1,u1,'ko','LineWidth',5);

set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
legend('Actual "humps" function',...
      'Piecewise linear polynomial','Piecewise quadratic polynomial');
print -depsc humps_quad.eps