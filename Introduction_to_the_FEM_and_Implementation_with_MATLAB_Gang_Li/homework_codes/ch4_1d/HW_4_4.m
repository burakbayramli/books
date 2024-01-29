clear all;

% next block: 4-th order polynomial
A=[1 -1 1 -1 1 
1 -1/2 (-1/2)^2 (-1/2)^3  (-1/2)^4  
1 0 0 0 0 
1 1/2 (1/2)^2 (1/2)^3  (1/2)^4
1 1 1 1 1 
];
b=[1/26 1/(1+25*(-1/3)^2) 1 1/(1+25*(1/2)^2)  1/26]';
a=A\b;     % compute the polynomial coefficients 

x=[-1.0:0.01:1.0];
exact=1./(1+25*x.^2);

u=a(1) +a(2)*x + a(3)*(x.^2) +a(4)*(x.^3) + a(5)*(x.^4);

% next block: 8-th order polynomial
A=[1 -1 1 -1 1 -1 1 -1 1 
1 -.75 (-.75)^2 (-.75)^3 (-.75)^4 (-.75)^5 (-.75)^6 (-.75)^7 (-.75)^8 
1 -.5 (-.5)^2 (-.5)^3 (-.5)^4 (-.5)^5 (-.5)^6 (-.5)^7 (-.5)^8 
1 -.25 (-.25)^2 (-.25)^3 (-.25)^4 (-.25)^5 (-.25)^6 (-.25)^7 (-.25)^8  
1 0 0 0 0 0 0 0 0 
1 .25 (.25)^2 (.25)^3 (.25)^4 (.25)^5 (.25)^6 (.25)^7 (.25)^8  
1 .5 (.5)^2 (.5)^3 (.5)^4 (.5)^5 (.5)^6 (.5)^7 (.5)^8 
1 .75 (.75)^2 (.75)^3 (.75)^4 (.75)^5 (.75)^6 (.75)^7 (.75)^8 
1 1 1 1 1 1 1 1 1 
];
b=[1/26  1/(1+25*(-.75)^2) 1/(1+25*(-.5)^2) 1/(1+25*(-.25)^2) ...
 1  1/(1+25*(.25)^2) ...
1/(1+25*(.5)^2) 1/(1+25*(.75)^2) 1/26]';

a=A\b; % compute the polynomial coefficients

u3=a(1) +a(2)*x + a(3)*(x.^2) + a(4)*(x.^3) ...
+ a(5)*(x.^4) + a(6)*(x.^5) + a(7)*(x.^6) ...
+ a(8)*(x.^7) + a(9)*(x.^8)  ;

% next block: plot exact function, 4-th and 8-th order polynomial
figure (1)
plot(x,exact, '-k', x,u,'--k',x,u3,'-.k','LineWidth',3);
hold on

% next block: piecewise quadratic interpolation
x= [-1 -0.75 -0.5 -0.25 0  0.25 0.5 0.75 1]';
u0=b;
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
hold off;
axis([-1 1 -1.3 1.3]);
xlabel('x');
ylabel('u(x)')
legend('Actual function','4-th order polynomial','8-th order polynomial'...
  ,'piecewise quadratic polynomial','Location','South');
set(gca,'fontsize',18);