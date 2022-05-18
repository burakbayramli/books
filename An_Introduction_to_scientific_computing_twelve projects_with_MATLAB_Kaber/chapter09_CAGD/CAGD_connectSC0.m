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
%%   C0 continuity connection of Bézier surfaces
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
clear all; close all;
%%  surface 1
np1=4;np2=5;
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
%% surface 2
np3=4;np4=5;
XP2=zeros(np3,np4);
YP2=zeros(np3,np4);
ZP2=zeros(np3,np4);
XP2=[ 2. , 2. , 2. , 2. , 2. ;
      3.5 , 3.5 , 3.5 , 3.5 , 3.5 ;
      4. , 4. , 4. , 4. , 4. ;
      5. , 5. , 5. , 5. , 5. ] ;
YP2=[ 0. , 1. , 2. , 3. , 4. ;
      0. , 1. , 2. , 3. , 4. ;
      0. , 1. , 2. , 3. , 4. ;
      0. , 1. , 2. , 3. , 4. ] ;
%%  different choices for ZP
%%  plane surface 
if (plan==1)
ZP2=[ 0. , 0. , 0. , 0. , 0. ;
      1. , 1. , 1. , 1. , 1. ;
      2. , 2. , 2. , 2. , 2. ;
      3. , 3. , 3. , 3. , 3. ] ;
end
%%  random surface
if (alea==1)
ZP2=rand(np3,np4);
end
%%  cylindric surface 
if (cyl==1)
 xc2=0.;yc2=4.;zc2=1.;r2=r1;
 for k1=1:np3
 r2=r2+(k1-1)*0.4;
 for k2=1:np4
   ddx=(XP2(k1,k2)-xc2)*(XP2(k1,k2)-xc2);
   ddy=(YP2(k1,k2)-yc2)*(YP2(k1,k2)-yc2);
   ZP2(k1,k2)=zc2+sqrt(r2*r2-ddy);
 end
 end
end
%%
%% definition of Bézier surfaces 
%%
T1=[0:0.05:1.];T2=[0:0.1:1.];
[X1,Y1,Z1,TRI1]=CAGD_sbezier(T1,XP1,YP1,ZP1);
[X2,Y2,Z2,TRI2]=CAGD_sbezier(T2,XP2,YP2,ZP2);
%%
%% display of Bézier surfaces 
%%
XP=[XP1,XP2];YP=[YP1,YP2];ZP=[ZP1,ZP2];
nf=1;
figure(nf)
hold on
xmin=min(min(XP));xmax=max(max(XP));
ymin=min(min(YP));ymax=max(max(YP));
zmin=min(min(ZP));zmax=max(max(ZP));
dx=0.1;dy=0.1;dz=0.1;
axis([xmin-dx,xmax+dx,ymin-dy,ymax+dy,zmin-dz,zmax+dz]);
CAGD_ubezier(TRI1,X1,Y1,Z1,XP1,YP1,ZP1,'P','k')
CAGD_ubezier(TRI2,X2,Y2,Z2,XP2,YP2,ZP2,'Q','k')
fs=18;title('C0 continuity connection','FontSize',fs);
hold off
