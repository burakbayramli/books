clear all;

L=3;
a=0;
n=200;   % number of points used to plot the shape function

x=[a:L/n:a+L]';
N1=-(x-a - L/3).*(x-a - 2*L/3).*(x-a - L)/(2/9*L^3);
N2=(x-a).*(x-a - 2*L/3).*(x-a - L)/(2/27*L^3);
N3=-(x-a).*(x-a - L/3).*(x-a - L)/(2/27*L^3); 
N4=(x-a).*(x-a - L/3).*(x-a - 2*L/3)/(2/9*L^3);

nodes= [a a+L/3 a+2*L/3 a+L]';
u=[0 0 0 0]';
hold off;
plot(nodes,u,'ko','LineWidth',5); % plot the nodes
hold on;

% next block: plot the shape functions
plot(x,N1,'b-','LineWidth',2); 
plot(x,N2,'r-','LineWidth',2); 
plot(x,N3,'k-','LineWidth',2); 
plot(x,N4,'g-','LineWidth',2); 

set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('u','fontsize',18);
legend('Data points','N_1(x)','N_2(x)','N_3(x)','N_4(x)');