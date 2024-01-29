clear all;

nodes= [-1 0 1]';  % three nodes
x=[-1:0.01:1]';    % points where shape functions and derivatives are evaluated
[N,Nx]=CompElementShapeQuadratic1D(nodes(1), nodes(2), nodes(3), x);

% next block: plot the shape functions
figure (1);
hold off;
plot(nodes,[0 0 0]','k-o','LineWidth',2); % plot the element
hold on;
plot(x,N(1,:),'r-','LineWidth',2); % plot N1(x)
plot(x,N(2,:),'b-','LineWidth',2); % plot N2(x)
plot(x,N(3,:),'g-','LineWidth',2); % plot N3(x)

set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('N_i(x)','fontsize',18);
legend('Element','N_1(x)', 'N_2(x)', 'N_3(x)'); 

% next block: plot the derivatives of the shape functions
figure (2);
hold off;
plot(nodes,[0 0 0]','k-o','LineWidth',2); % plot the element
hold on;
plot(x,Nx(1,:),'r-','LineWidth',2); % plot dN1(x)/dx
plot(x,Nx(2,:),'b-','LineWidth',2); % plot dN2(x)/dx
plot(x,Nx(3,:),'g-','LineWidth',2); % plot dN3(x)/dx

set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('N''_i(x)','fontsize',18);
legend('Element','N''_1(x)', 'N''_2(x)', 'N''_3(x)'); 