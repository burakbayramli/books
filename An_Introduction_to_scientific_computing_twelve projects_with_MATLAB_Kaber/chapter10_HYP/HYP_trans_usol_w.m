%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%=======================================
% computes    W=(rho, rho*U, E) 
% from     usol=(rho, U, p)  
%=======================================

function w = HYP_trans_usol_w(usol)

global gamma aL pL rhoL aR pR rhoR dum1 dum2 dum3;

w(1,:)=usol(1,:);                 
w(2,:)=usol(1,:).*usol(2,:);
w(3,:)=usol(3,:)/(gamma-1)+0.5*w(2,:).*usol(2,:);

