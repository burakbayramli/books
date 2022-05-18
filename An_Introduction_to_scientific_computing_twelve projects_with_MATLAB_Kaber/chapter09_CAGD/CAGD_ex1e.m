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
%%   Construction of a Bézier curve
%%   by using De Casteljau algorithm
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
clear all; close all;
% control points definition
np=5;
XP=zeros(np,1);YP=zeros(np,1);
XP=[ 0. , 1. , 2. , 3. , 3.5 ] ;
YP=[ 0. , 2.5 , 3. , 1.5 , 0. ] ;
% sampling the Bézier curve
T=[0:0.05:1.];
n=size(T,2);
X=zeros(n,1);Y=zeros(n,1);
for k=1:n
t=T(k);
[x,y]=CAGD_casteljau(t,XP,YP);
X(k)=x;Y(k)=y;
end
%
% graphics
%
% a) window definition 
nf=1;figure(nf) ; hold on; 
xmin=min(XP)-0.5;xmax=max(XP)+0.5;
ymin=min(YP)-0.5;ymax=max(YP)+0.5;
axis([xmin,xmax,ymin,ymax]);
% b) curve display 
fs1=14;
set(gca,'FontSize',fs1)
set(gca,'FontSize',fs1)
plot(X,Y,'b',XP,YP,'r--')
% c) construction display 
for k=1:np
kk=k-1;
char=int2str(kk);
P=strcat('P',char);
epsx=0.05;epsy=0.2;
if (k==1) epsx=0.; epsy=-0.2; end
if (k==np) epsx=0.; epsy=-0.2; end
text(XP(k)+epsx,YP(k)+epsy,P);
end
xx=XP;yy=YP;
m=np-1;
t=0.5;
for kk=1:m
xxx=xx;yyy=yy;
for k=kk:m
zx(1)=xxx(k);zx(2)=xxx(k+1);
zy(1)=yyy(k);zy(2)=yyy(k+1);
if (kk>1) 
plot(zx,zy,'k');
end  
xx(k+1)=(1-t)*xxx(k)+t*xxx(k+1);
yy(k+1)=(1-t)*yyy(k)+t*yyy(k+1);
if (kk==m) 
   epsx=0.;epsy=-0.25;
   text(xx(k+1)+epsx,yy(k+1)+epsy,'P(t)');
   text(xx(k+1),yy(k+1),'x');
end
end
end
fs=18;title('De Casteljau algorithm','FontSize',fs); hold off;
