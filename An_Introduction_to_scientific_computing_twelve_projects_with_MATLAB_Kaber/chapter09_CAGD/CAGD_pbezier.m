%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function CAGD_pbezier(X,Y,XP,YP,color,pchar,pcolor,ptrait);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  function CAGD_pbezier(X,Y,XP,YP,color,pchar,pcolor,ptrait)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Display of a Bézier curve with control polygon
%%   
%%   Input : X, Y sampling points coordinates
%%           XP, YP control points coordinates
%%           color  curve color
%%           pchar  control point character
%%           pcolor control polygon color
%%           ptrait control polygon line type
%%
%%   Ouput : Display of the curve and control polygon
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
line=strcat(pcolor,ptrait);
XP0=[ XP , XP(1) ] ;
YP0=[ YP , YP(1) ] ;
plot(X,Y,color,XP0,YP0,line)
np=size(XP,2);
for k=1:np
kk=k-1;
char=int2str(kk);
P=strcat(pchar,char);
epsx=0.1;epsy=0.2;
if (k==1) epsx=0.; epsy=-0.2; end
if (k==np) epsx=0.2; epsy=0.; end
fs=14;text(XP(k)+epsx,YP(k)+epsy,P,'FontSize',fs);
end
