%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function y=ODE_fun2(t,x)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  function y=ODE_fun2(t,x)
%%  Exercise 2.2
%%  Right hand side function for the nonlinear differentiel system
%%  with  2 equations
%%  U(t)=f(t,U), U:R->RxR,   f:[0,+infinity[xRxR -> RxR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global A
global B
%% The values of parameters A and B are  fixed in the main script
%% Chemistry2.m
y=[A+x(1)^2*x(2)-(B+1)*x(1);B*x(1)-x(1)^2*x(2)];
