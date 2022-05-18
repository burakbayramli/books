%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Matlab Solution of exercise 3 - project 9
%%   CAGD: geometrical design
%%   Construction of Bézier surface
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
clear all; close all;
np1=4;np2=5;
% control points definition (rectangular grid)
XP=zeros(np1,np2);YP=zeros(np1,np2);ZP=zeros(np1,np2);
XP=[ 0. , 0. , 0. , 0. , 0. ;
     1. , 1. , 1. , 1. , 1. ;
     2. , 2. , 2. , 2. , 2. ;
     3. , 3. , 3. , 3. , 3. ] ;
YP=[ 0. , 1. , 2. , 3. , 4. ;
     0. , 1. , 2. , 3. , 4. ;
     0. , 1. , 2. , 3. , 4. ;
     0. , 1. , 2. , 3. , 4. ] ;
%%  different choices for ZP
%%  Plane surface
ZP=[ 0. , 0. , 0. , 0. , 0. ;
     1. , 1. , 1. , 1. , 1. ;
     2. , 2. , 2. , 2. , 2. ;
     3. , 3. , 3. , 3. , 3. ] ;
%%  Random surface
ZP=rand(np1,np2);
%%  Cylindric surface
for k1=1:np1
for k2=1:np2
d=(XP(k1,k2)-1.)*(XP(k1,k2)-1.);
d=d+(YP(k1,k2)-0.5)*(YP(k1,k2)-0.5);
ZP(k1,k2)=sqrt(d);
end
end
% sampling the Bézier surface
%
T=[0:0.05:1.];
[X,Y,Z,TRI]=CAGD_sbezier(T,XP,YP,ZP);
%%
%% surface display
%%
nf=1; figure(nf) ; hold on ;
xmin=min(min(XP));xmax=max(max(XP));
ymin=min(min(YP));ymax=max(max(YP));
zmin=min(min(ZP));zmax=max(max(ZP));
dx=0.1;dy=0.1;dz=0.1;
axis([xmin-dx,xmax+dx,ymin-dy,ymax+dy,zmin-dz,zmax+dz]);
CAGD_ubezier(TRI,X,Y,Z,XP,YP,ZP,'P','k')
title('Bézier surface'); hold off ;