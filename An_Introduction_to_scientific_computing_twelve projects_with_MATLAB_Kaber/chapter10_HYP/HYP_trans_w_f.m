%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%=======================================
% computes   F=(rho*U, rho*U^2+p, (E+p)*U)  
% from       W=(rho, rho U, E)
%=======================================

function f = HYP_trans_w_f(w)

global gamma aL pL rhoL aR pR rhoR dum1 dum2 dum3;

f(1,:)= w(2,:); 
uloc  = w(2,:)./w(1,:);
rhou2 = w(2,:).*uloc;
press = (gamma-1)*(w(3,:)-0.5*rhou2);
f(2,:)= rhou2+press;
f(3,:)= (w(3,:)+press).*uloc;

