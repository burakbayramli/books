clear all;

x= [0 1 4 9 16 25 36 49 64]';
u0=[0 1 2 3 4 5 6 7 8 ]';
hold off;
plot(x,u0,'ko','LineWidth',5); % plot the data points
hold on;

% next block: Lagrange interpolation with single polynomial
dp(:,1)=x;
dp(:,2)=u0;
x_vector=[0:0.05:64]';
u=CompLagIntp1D(dp, x_vector);   % single polynomial
plot(x_vector,u,'b-','LineWidth',2); % plot polynomial

% next line: plot the sqrt function
plot(x_vector, sqrt(x_vector),'k-','LineWidth',2);

% next block: piecewise Hermite interpolation
% Note: first derivative of sqrt(x) does not exist
% at zero, we start at 0.01 as an approximation 
xh=[0.01 1 4 9 16 25 36 49 64]';
u0=[0.1 1 2 3 4 5 6 7 8 ]';
up=[5 0.5 0.25 0.1667 0.125 0.1 0.0833 0.0714 0.0625]';
i=1;
while i+1<=size(x,1)
  x0=xh(i); x1=xh(i+1);
  uh0=u0(i); uh1=u0(i+1);
  up0=up(i); up1=up(i+1);
  dx=x1-x0;
  x_vector=[x0:dx/100:x1]';
  u=(1+2.*(x_vector-x0)/dx).*((x1-x_vector)/dx).^2*uh0 ...
    + (x_vector-x0).*((x1-x_vector)/dx).^2*up0 ...
    + (1+2.*(x1-x_vector)/dx).*((x_vector-x0)/dx).^2*uh1 ...
    + (x_vector-x1).*((x_vector-x0)/dx).^2*up1;
  plot(x_vector,u,'r-.','LineWidth',2); % plot polynomial
  i=i+1;
end

% next block: figure labels and legend
set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
legend('Data points','Single polynomial of degree 8',...
       'Actual sqrt function','Piecewise Hermite interpolation',...
       'Location','NorthWest');