clear all;

A=[1 2 4 8
0 1 4 12
1 4 16 64
0 1 8 48]

b=[5 1 10 -2]';

a=A\b

x=[2.0:0.1:4.0];

u=a(1) +a(2)*x + a(3)*(x.^2) +a(4)*(x.^3);
up=     a(2)+ 2*a(3)*x + 3*a(4)*(x.^2);

figure (1)

plot(x,u,'-k','LineWidth',2);
xlabel('x');
ylabel('u(x)')
set(gca,'fontsize',18);
%set(gca,'units','points');
%set(gca,'position',[58 60 400 300]);

print -depsc -r150 hermite_example2.eps

%figure(2)
%plot(x,up,'-');
