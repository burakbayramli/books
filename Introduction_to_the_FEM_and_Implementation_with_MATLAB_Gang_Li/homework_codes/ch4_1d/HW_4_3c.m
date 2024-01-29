clear all;

nodes= [0 1 2 3]';
u0=[3 5 1 2]';
hold off;
plot(nodes,u0,'ko','LineWidth',5); % plot the data points
hold on;
plot(nodes,[0 0 0 0]','k-o','LineWidth',2); % plot the element

% next block: Lagrange interpolation 
dp(:,1)=nodes;
dp(:,2)=u0;
x=[0:0.05:3]';
u=CompLagIntp1D(dp, x);   % cubic polynomial
plot(x,u,'r--','LineWidth',2); % plot polynomial

% next block: monomial basis
A=[1 0 0 0
   1 1 1 1 
   1 2 4 8
   1 3 9 27];
c=A\u0;        % compute the polynomial coefficients
u=c(1)+c(2).*x + c(3).*x.^2 + c(4).*x.^3;   % cubic polynomial
plot(x,u,'b:','LineWidth',2); % plot polynomial
axis([0 3 -1 7]);
set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
legend('Data points','Element',...
       'Cubic polynomial (Lagrange basis)', ...
       'Cubic polynomial (Monomial basis)');