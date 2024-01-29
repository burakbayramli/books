clear all;

n=10;      % number of intervals

It=zeros(n,1);
Is=zeros(n,1);
for i=1:n         % for all the decomposition schemes
  h=1/i;          % interval
  pt=[0:h:1];     % Trapezoid rule integration points
  ps=[0:h/2:1];   % Simpson's rule integration points
  for j=1:i
    wt=[0.5*h 0.5*h];
    xt=pt(j:j+1);
    It(i)=It(i)+sum(4./(1+xt.^2).*wt);  
    ws=[1/6*h 4/6*h 1/6*h];
    xs=ps(2*j-1:2*j+1);
    Is(i)=Is(i)+sum(4./(1+xs.^2).*ws);
  end
end

Ie=3.1415926535;  % exact result
  
hold off;
plot([1:n],It,'b-o','LineWidth',3); % plot the data points
hold on;
plot([1:n],Is,'r-o','LineWidth',3); % plot the data points
plot([1 n],[Ie Ie],'k-','LineWidth',3); % plot the data points

axis([1 n 2.95 3.2]);
set(gca,'fontsize',16);
xlabel('Divisions','fontsize',18);
ylabel('Integration result','fontsize',18);
legend('Trapezoid rule','Simpson''s rule','Exact result');
       