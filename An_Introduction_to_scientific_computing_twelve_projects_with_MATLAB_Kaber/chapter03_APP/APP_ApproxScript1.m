%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%
n=10;x=sort(rand(n+1,1));
%
A=ones(length(x),1);
for k=1:length(x)-1
A=[A x.^k];
end;
%
test1=inline('sin(10.*x.*cos(x))')
%
b=test1(x);
cf=A\b;
cf=cf(end:-1:1);%reordering of the coefficients
xx=linspace(0,1,100);
yy= polyval(cf,xx);
y= polyval(cf,x);
xx=linspace(0,1,100);
plot(xx,test1(xx),xx,yy,x,y,'r+');
legend('f','I_nf','xy_i')
title('polynomial approximation of sin(10.*x.*cos(x))')
%
cf=cf(end:-1:1);%reordering of the coefficients
fprintf('norm(A*cf-b)=%f\n', norm(A*cf-b));
fprintf('cond(A) =%f\n',cond(A))
fprintf('rank(A) =%d\n',rank(A) )
 
