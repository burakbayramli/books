%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% ===========================
% exact solution
% for the convection equation
%   du/dt +c du/dx= 0
%====================================================
% Input arguments :
%      a,b  the definition interval [a,b]
%      c>0  the convection speed
%      x    the vector x(j)=a+j*delta x, j=0,1,...,J
%      T    the time at which the solution is computed
%      fun_in(x)   initial condition for t=0
%      fun_bc(x)   boundary condition for x=a
% Output argument :
%      uex   the vector of length J+1 containing the
%      exact solution
%=====================================================

function uex=PDE_conv_exact_sol(a,b,c,x,T,fun_in,fun_bc)

uex=x;             % initialization (same dimensions as x)

car=x-c*T;         % vector containing the characteristics

ip=find(car>=a);   % the solution depends on the initial condition
uex(ip)=feval(fun_in,car(ip));

in=find(car<a);    % the solution depends on the boundary condition
uex(in)=feval(fun_bc,T-(x(in)-a)/c);
