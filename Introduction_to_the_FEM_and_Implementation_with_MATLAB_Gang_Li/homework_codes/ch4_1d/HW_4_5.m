clear all;

K=[2 -2  0 
  -2  4 -2 
   0 -2  2];
   
f=[-11/12 1/2 5/12]';

K(3,3)=2e6;
f(3)=0;

u=K\f

x1=[0:1/100:1];                    % analytically obtain the exact  
exact_u=-1/3 * x1.^3 + x1 - 2/3;   % solutions for comparison

% next block: plot nodal u, compare with the exact solution
figure(1);                                     % figure 1
plot([0 1/2 1], u,'s-',x1,exact_u,'-', 'LineWidth',2);  
xlabel('x');                                  
ylabel('u');
legend('FE nodal solution', 'Exact solution','location','northwest');