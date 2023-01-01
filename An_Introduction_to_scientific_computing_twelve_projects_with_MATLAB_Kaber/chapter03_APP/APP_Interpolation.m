%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
n=10;
%x=(-n:n)'/n;               % The uniform case
x=cos(pi*(.5+n:-1:0)/(n+1));% The Chebyshev case
g=-1:0.02:1;
c=APP_dd(x);y=APP_interpole(c,x,g);
yg=APP_f(g);plot(g,yg,g,y,'r+','LineWidth',2,'MarkerSize',10)
set(gca,'XTick',-1:.4:1,'FontSize',24)
%set(gca,'YTick',-1:.2:1)
hold on;yx=APP_f(x);plot(x,yx,'O');hold off
legend('u','I_n u','interpolation points')


 