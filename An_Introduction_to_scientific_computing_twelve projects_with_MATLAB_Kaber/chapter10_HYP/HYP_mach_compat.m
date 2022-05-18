%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%==============================================
% compatibility relation for the shock tube problem
%==============================================

function y=HYP_mach_compat(x)

global gamma aL pL rhoL aR pR rhoR dum1 dum2 dum3;

y=(x-1./x)-aL/dum2*(1-(pR/pL*(dum1*gamma*x.*x-dum2)).^(dum3/gamma));
