%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [XPP,YPP]=CAGD_cast2(t,XP,YP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [XPP,YPP]=CAGD_cast2(t,XP,YP)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Convex hull of a Bézier curve
%%   Construction of control points 
%%   Second step: t in [0.5,1.]
%%   
%%   Input  : t  parameter value 
%%            XP, YP control points coordinates
%%
%%   Output : XPP, YPP control points coordinates 
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  
m=size(XP,2)-1;
for k=1:m+1
   xx(m+2-k)=XP(k);
   yy(m+2-k)=YP(k);
end
for kk=1:m
xxx=xx;
yyy=yy;
XPP(m+2-kk)=xx(kk);YPP(m+2-kk)=yy(kk);
for k=kk:m
xx(k+1)=t*xxx(k)+(1.-t)*xxx(k+1);
yy(k+1)=t*yyy(k)+(1.-t)*yyy(k+1);
end
end
XPP(1)=xx(m+1);YPP(1)=yy(m+1);

