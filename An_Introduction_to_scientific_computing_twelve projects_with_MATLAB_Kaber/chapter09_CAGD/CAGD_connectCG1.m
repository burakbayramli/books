%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Matlab Solution of exercise 1 - project 9
%%   CAGD: geometrical design
%%   G1 continuity connection of Bézier curves
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
clear all; close all;
% control points
np1=5;
XP1=zeros(np1,1);YP1=zeros(np1,1);
XP1=[ 0. , 1. , 2. , 3. , 3.5   ] ;
YP1=[ 0. , 2.5 , 3. , 1.5 , 0.   ] ;
np2=4;
XP2=zeros(np2,1);YP2=zeros(np2,1);
XP2=[ 3.5,   4.5   , 5. , 6.  ] ;
YP2=[ 0. ,  -3. , -4. , 0.  ] ;
% Bézier curves
T=[0:0.05:1.];
[X1,Y1]=CAGD_cbezier(T,XP1,YP1);
[X2,Y2]=CAGD_cbezier(T,XP2,YP2);
nf=1;
figure(nf)
hold on
xmin1=min(XP1);xmax1=max(XP1);
ymin1=min(YP1);ymax1=max(YP1);
xmin2=min(XP2);xmax2=max(XP2);
ymin2=min(YP2);ymax2=max(YP2);
xmin=min(xmin1,xmin2)-0.75;xmax=max(xmax1,xmax2)+0.75;
ymin=min(ymin1,ymin2)-0.75;ymax=max(ymax1,ymax2)+0.75;
axis([xmin,xmax,ymin,ymax]);
plot(X1,Y1,'b',XP1,YP1,'b--')
plot(X2,Y2,'r',XP2,YP2,'r--')
for k=1:np1
kk=k-1;
char=int2str(kk);
P=strcat('P',char);
epsx=0.1;epsy=0.2;
if (k==1) epsx=0.; epsy=-0.2; end
if (k==np1) epsx=0.2; epsy=0.; end
fs=14;text(XP1(k)+epsx,YP1(k)+epsy,P,'FontSize',fs);
end
for k=1:np2
kk=k-1;
char=int2str(kk);
P=strcat('P''',char);
epsx=-0.4;epsy=-.2;
if (k==1) epsx=-0.4; epsy=0.; end
if (k==np2) epsx=0.1; epsy=0.; end
fs=14;text(XP2(k)+epsx,YP2(k)+epsy,P,'FontSize',fs);
end
% tangent vectors 
plot(XP1(np1-1:np1),YP1(np1-1:np1),'b')
plot(XP2(1:2),YP2(1:2),'r');fs=18;
title('G1 continuity connection','FontSize',fs);
hold off
