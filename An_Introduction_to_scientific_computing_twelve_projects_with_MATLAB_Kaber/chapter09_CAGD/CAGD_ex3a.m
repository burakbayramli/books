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
%%   Plot of a Bézier patch
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
clear all; close all;
np1=4;np2=5;
% control points definition (rectangular grid)
XP1=zeros(np1,np2);
YP1=zeros(np1,np2);
ZP1=zeros(np1,np2);
XP1=[ 0. , 0. , 0. , 0. , 0. ;
      .5 , .5 , .5 , .5 , .5 ;
      1. , 1. , 1. , 1. , 1. ;
      2. , 2. , 2. , 2. , 2. ] ;
YP1=[ 0. , 1. , 2. , 3. , 4. ;
      0. , 1. , 2. , 3. , 4. ;
      0. , 1. , 2. , 3. , 4. ;
      0. , 1. , 2. , 3. , 4. ] ;
%%  different choices for ZP
%%  plane surface 
plan=0;
if (plan==1)
ZP1=[ 0. , 0. , 0. , 0. , 0. ;
      1. , 1. , 1. , 1. , 1. ;
      2. , 2. , 2. , 2. , 2. ;
      3. , 3. , 3. , 3. , 3. ] ;
end
%%  random surface 
alea=0;
if (alea==1)
ZP1=rand(np1,np2);
end
%%  cylindric surface 
cyl=1;
if (cyl==1)
 xc1=0.;yc1=4.;zc1=1.;r1=4.;
 for k1=1:np1
 r1=r1-(k1-1)*0.2;
 for k2=1:np2
   ddx=(XP1(k1,k2)-xc1)*(XP1(k1,k2)-xc1);
   ddy=(YP1(k1,k2)-yc1)*(YP1(k1,k2)-yc1);
   ZP1(k1,k2)=zc1+sqrt(r1*r1-ddy);
  end
  end
end
%%
%% definition of Bézier patch
%%
T1=[0:0.05:1.];
[X1,Y1,Z1,TRI1]=CAGD_sbezier(T1,XP1,YP1,ZP1);
%%
%% display of Bézier patch 
%%
nf=1;figure(nf);hold on
xmin=min(min(XP1));xmax=max(max(XP1));
ymin=min(min(YP1));ymax=max(max(YP1));
zmin=min(min(ZP1));zmax=max(max(ZP1));
dx=0.1;dy=0.1;dz=0.1;
axis([xmin-dx,xmax+dx,ymin-dy,ymax+dy,zmin-dz,zmax+dz]);
CAGD_ubezier(TRI1,X1,Y1,Z1,XP1,YP1,ZP1,'P','k')
fs=18;title('Example of Bézier patch','FontSize',fs);
hold off
