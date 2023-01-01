%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%=======================================
% computation of the time step
%   dt=cfl*dx/(|U|+a)
% from the components of  W=(rho, rho*U, E)
%=======================================

function dt = HYP_calc_dt(w,dx,cfl)

global gamma aL pL rhoL aR pR rhoR dum1 dum2 dum3;

uloc  = w(2,:)./w(1,:);
press = (gamma-1)*(w(3,:)-0.5*w(2,:).*uloc);
a=(gamma*press./w(1,:)).^0.5;

dt=cfl*dx/max(abs(uloc)+a);

