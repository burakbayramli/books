%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise 5.3
%% Compute and displays Legendre expansion of a function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close all
Test1=inline('sin(6*x).*exp(-x)','x');
Test2=inline('abs(x)','x');
Test3=inline('sign(x)','x');
s=100; P=30;
npt=200;

figure
[x,y9,err]=SPE_CalcLegExp(s,P,npt,Test1);
fprintf('for f(x)=sin(6*x).*exp(-x) ||f-L_9||_inf=%f\n',err)
s=100; P=6;
[x,y6,err]=SPE_CalcLegExp(s,P,npt,Test1);
plot(x,Test1(x),'-',x,y6,'--',x,y9,':')
legend('function','L_6','L_9')

figure
npt=200;
[x,y,err]=SPE_CalcLegExp(s,30,npt,Test2);
fprintf('for f(x)=abs(x) ||f-L_30||_inf=%f\n',err)
plot(x,Test2(x),'-',x,y,'--')
legend('sign','L_{30}(abs)')
xlabel('x')
  
figure
npt=200;
[x,y,err]=SPE_CalcLegExp(s,30,npt,Test3);
fprintf('for f(x)=sign(x) ||f-L_30||_inf=%f\n',err)
plot(x,Test3(x),'-',x,y,'--')
legend('sign','L_{30}(sign)')
xlabel('x')
  
