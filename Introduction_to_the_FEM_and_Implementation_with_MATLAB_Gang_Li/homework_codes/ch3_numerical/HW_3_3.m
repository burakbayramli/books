clear all;

x= [1 2 3 4 5]';
u0=[1 1 2 6 24]';
hold off;
plot(x,u0,'ko','LineWidth',5); % plot the data points
hold on;

% next block: Lagrange interpolation with single polynomial
dp(:,1)=x;
dp(:,2)=u0;
x_vector=[1:0.05:5]';
u=CompLagIntp1D(dp, x_vector);   % single polynomial
plot(x_vector,u,'b-','LineWidth',2); % plot polynomial

% next line: piecewise linear interpolation: connecting 
% the data points
plot(x,u0,'r--','LineWidth',2);

% next line: plot the actual gamma function
plot(x_vector,gamma(x_vector),'k-','LineWidth',2);

% next block: figure labels and legend
set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
legend('Data points','Single polynomial of degree 4',...
       'piecewise linear polynomials', ...
       'Actual gamma function','Location','Northwest');