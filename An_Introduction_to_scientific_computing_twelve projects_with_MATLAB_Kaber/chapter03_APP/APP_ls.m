%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function APP_ls()
%least squares  approximation
%on I=[0 1] of the function f
%
close all
m=10;
x=(0:m)'/m;y=APP_f(x);
n=0;p=polyfit(x,y,n);
%compute the error
norm0=norm(y-polyval(p,x));
fprintf('For n = %i :: norm of the error = %e \n',n,norm(y-polyval(p,x)));
goon=1;tol=.5;
while goon & n<11
    p=polyfit(x,y,n);
    norm1=norm(y-polyval(p,x));
    goon =abs(norm1-norm0) < norm0*tol;
    n=n+1;norm0=norm1;
end    
fprintf(' n = %i, error = %g \n',n,norm1);
%graphics
I=linspace(0,1,100);
f=polyval(p,I); 
hold off;plot(I,f,'LineWidth',2,'MarkerSize',10)
set(gca,'XTick',0:.2:1,'FontSize',24);
%set(gca,'YTick',-1:.2:1,'FontSize',24);
hold on;
plot(I,APP_f(I),'LineWidth',2,'Color',[0.9,0.2,0.3])
plot(x,y,'+','MarkerSize',10)
legend('p_n','f','xy_i')
title('Least squared approximation')
