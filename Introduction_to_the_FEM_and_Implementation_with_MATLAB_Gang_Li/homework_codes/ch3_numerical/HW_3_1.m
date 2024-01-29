clear all;

x= [0.0 0.5 1.0 6.0 7.0 9.0]';
u0=[0.0 1.6 2.0 2.0 1.5 0.0]';
hold off;
plot(x,u0,'ko','LineWidth',5); % plot the data points
hold on;

% next block: Lagrange interpolation with single polynomial
dp(:,1)=x;
dp(:,2)=u0;
x_vector=[0:0.05:9]';
u=CompLagIntp1D(dp, x_vector);   % single polynomial
plot(x_vector,u,'b-','LineWidth',2); % plot polynomial

% next line: piecewise linear interpolation: connecting 
% the data points
plot(x,u0,'k--','LineWidth',2);

% next block: piecewise quadratic interpolation
i=1;
dp=zeros(3,2);
while i+2<=size(x,1)
  dp(:,1)=x(i:i+2);
  dp(:,2)=u0(i:i+2);
  x_vector=[x(i):0.05:x(i+2)]';
  u=CompLagIntp1D(dp, x_vector);   % quadratic polynomial
  plot(x_vector,u,'r-.','LineWidth',2); % plot polynomial
  i=i+2;
end

% next block: figure labels and legend
set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
legend('Data points','Single polynomial of degree 5',...
       'piecewise linear polynomials', ...
       'piecewise quadratic polynomials');