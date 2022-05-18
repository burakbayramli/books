%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function CAGD_ubezier(TRI,X,Y,Z,XP,YP,ZP,pchar,color)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function  CAGD_ubezier(TRI,X,Y,Z,XP,YP,ZP,pchar,color)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%   Display of a Bézier surface with control points
%%   
%%   Input : TRI list of triangular faces of the surface
%%           X, Y, Z sampling points coordinates
%%           XP, YP, ZP control points coordinates
%%           pchar  control point character
%%           pcolor control polygon color
%%
%%   Ouput : Display the surface and control points
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
tcolor=strcat(color,'--');
% surface display
trisurf(TRI,X,Y,Z)
% control points display
texte=1;
if (texte==1)
np1=size(XP,1);np2=size(XP,2);
%%
for k1=1:np1
kk1=k1-1;
char1=int2str(kk1);
PP=strcat(pchar,char1);
for k2=1:np2
kk2=k2-1;
char2=int2str(kk2);
P=strcat(PP,char2);
epsx=0.05;epsy=0.05;;epsz=0.05;
text(XP(k1,k2)+epsx,YP(k1,k2)+epsy,ZP(k1,k2)+epsz,P);
end
end
%%
end