%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%==========================
% Gaussian applied on 
% M=[x1  x2   x3
%    y1  y2   y3]
% XYres=[xr
%      yr]
%==========================

function FM=Res_Gauss(M,XYres,Rr)
M=M-XYres*ones(1,size(M,2));
fact=0.5/(Rr*Rr);
FM=0.5*exp(-fact*sum(M.*M));