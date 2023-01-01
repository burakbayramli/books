%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%=======================================
% computes    usol=(rho, U, p)   
% from           W=(rho, rho*U, E)
%=======================================

function usol = HYP_trans_w_usol(w)

global gamma aL pL rhoL aR pR rhoR dum1 dum2 dum3;

usol(1,:)=w(1,:);                 
usol(2,:)=w(2,:)./w(1,:);
usol(3,:)=(gamma-1)*(w(3,:)-0.5*w(2,:).*usol(2,:));

