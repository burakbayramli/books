
clear all;

x= [0 1 2 3 4]';
u0=[1.0 2.7 5.8 6.6 7.5]';
hold off;
plot(x,u0,'ko','LineWidth',5); % plot the data points
hold on;

% next block: Lagrange interpolation with single polynomial
dp(:,1)=x;
dp(:,2)=u0;
x_vector=[0:0.05:4]';
u=CompLagIntp1D(dp, x_vector);   % single polynomial
plot(x_vector,u,'b-','LineWidth',2); % plot polynomial

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

figure (1);
set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
legend('Data points','Single polynomial of degree 4',...
       'piecewise quadratic polynomials','Location','SouthEast');


figure(2);
hold off;
plot(0,0,'r.');
hold on;

% for-loop: generate 100 sets of points with +-0.5 perturbation to u0
% and use two interpolation methods to interpolate the data
for i=1:100
  u0r=u0+ rand(5,1)-0.5;  % add +-0.5 perturbation to u0 
  % next block: Lagrange interpolation derivatives
  dp=zeros(5,2);
  dp(:,1)=x;
  dp(:,2)=u0r;
  x_vector=[0 1 2 3 4]';
  ux=CompLagIntp1DDerivative(dp, x_vector);
  plot(x_vector,ux,'ro','LineWidth',2); 

  % next block: piecewise quadratic interpolation
  i=1;
  dp=zeros(3,2);
  while i+2<=size(x,1)
    dp(:,1)=x(i:i+2);
    dp(:,2)=u0r(i:i+2);
    x_vector=[x(i) x(i+1) x(i+2)]';
    ux=CompLagIntp1DDerivative(dp, x_vector);   
    i=i+2;
    plot(x_vector,ux,'bo','LineWidth',2); 
  end
end