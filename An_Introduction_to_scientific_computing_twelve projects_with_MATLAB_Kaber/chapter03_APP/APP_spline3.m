%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
close all
f=inline('sin(4*pi*x)');
n=5;x=(0:n)'/n;h=1/n;fx=f(x);
b=-2*fx(2:n)+fx(1:n-1)+fx(3:n+1);
b=6*b/h;A=h*toeplitz([4,1,zeros(1,n-3)]);
alpha=A\b;alpha=[0;alpha;0];
%Evaluation of $p_i$ on each interval $[x_i,x_{i+1}]$
hold on
for i=1:n
   a=(alpha(i+1)-alpha(i))/6/h;
   b=alpha(i)/2;
   c=(fx(i+1)-fx(i))/h-(2*alpha(i)+alpha(i+1))*h/6;
   Ii=linspace(x(i),x(i+1),20);
   fi=f(Ii);
   Si=a*(Ii-x(i)).^3+b*(Ii-x(i)).^2+c*(Ii-x(i))+fx(i);
   plot(Ii,fi,Ii,Si,'r+','MarkerSize',10,'LineWidth',3);
   set(gca,'FontSize',24);
   fprintf('Hit any key to go on\n')
   title(strcat('intervals 1..',num2str(i)))
   xlim([0,1])
   pause
end
title('Cubic spline interpolation of sin(4*pi*x) ')
legend('f','S_3')
