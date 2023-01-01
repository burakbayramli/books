%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise 5.3 question 5)
%% convergence of  Legendre  expansion
%% for various test functions, display the  error in supremum norm 
%% between the  function and its Legendre expansion, versus the degree of the  
%% expansion.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Test=inline('sin(6*x).*exp(-x)','x');   % test function selection, 
%  For a smooth function, one expects exponential convergence.
Test=inline('abs(x)','x');           
Test=inline('sign(x)','x');           
close all 
s=100;         % degree of Gauss quadrature used to compute the 
%      coefficients of the expansion. The same quadrature is used
%      for all degrees expansions.
PP=[2:2:30];   % degree of the expansions.
E=[];          % initialization of the array containing the 
% errors between the expansion and the function.
for P=PP
  [x,y,err]=SPE_CalcLegExp(s,P,100,Test);   
   E=[E,err];
end
figure
%plot(PP,log(E),'-or')
plot(1.0./PP,E,'-or')
xlabel('p')
legend('||f-L_p(f)||_\infty',2)
title('convergence of Legendre approximation for sin(6*x).*exp(-x)')


