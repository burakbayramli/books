%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [x,y,z]=CAGD_cast3d(t,XP,YP,ZP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [x,y,z]=CAGD_cast3d(t,XP,YP)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Construction of point P(t) of a Bézier curve
%%   by using de Casteljau algorithm.
%%   
%%   Input : t  parameter value
%%           XP, YP, ZP control points coordinates
%%
%%   Ouput : x,y,z coordinates of P(t) in R^3
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
m=size(XP,1)-1;
xx=XP;yy=YP;zz=ZP;
for kk=1:m
xxx=xx;yyy=yy;zzz=zz;
for k=kk:m
xx(k+1)=(1-t)*xxx(k)+t*xxx(k+1);
yy(k+1)=(1-t)*yyy(k)+t*yyy(k+1);
zz(k+1)=(1-t)*zzz(k)+t*zzz(k+1);
end
end
x=xx(m+1);y=yy(m+1);z=zz(m+1);