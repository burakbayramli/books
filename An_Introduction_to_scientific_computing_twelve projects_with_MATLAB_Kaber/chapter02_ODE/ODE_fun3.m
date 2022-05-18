%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function y=ODE_fun3(t,x)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  function y=ODE_fun3(t,x)
%%  Exercice 2.4
%%  Right hand side function for the nonlinear differentiel system
%%  with  3 equations
%%  U(t)=f(t,U), U:R->RxRxR,   f:[0,+infinity[xRxRxR -> RxRxR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global v
y=[1+x(1)^2*x(2)-(x(3)+1)*x(1);x(3)*x(1)-x(1)^2*x(2);-x(1)*x(3)+v];

