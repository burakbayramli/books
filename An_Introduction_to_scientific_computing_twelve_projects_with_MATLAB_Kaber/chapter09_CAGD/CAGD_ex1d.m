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
%%   Construction of some Bézier curves
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Examples of multiple control points use
%%
clear all; close all;
% control points definition
np=5;shift=5;
XP=zeros(np,1);YP=zeros(np,1);
XP=[ 0. , 1.  , 2. , 3. , 3.5   ] ;
YP=[ 0. , 2.5 , 3. , 1.5 , 0.   ] ;
%
% one more point to close the curve
%
np=np+1;
XP=[ XP , XP(1) ] ;
YP=[ YP , YP(1) ] ;
% sampling the Bézier curve
T=[0:0.01:1.];
[X,Y]=CAGD_cbezier(T,XP,YP);
%
% graphics
%
% a) window definition 
nf=11; figure(nf) ; hold on ; fs=18;
xmin=min(XP)-0.5;xmax=max(XP)+0.5;
ymin=min(YP)-0.5;ymax=max(YP)+0.5;
axis([xmin,xmax,ymin,ymax]);
% b) curve display 
color='r';pchar='P';pcolor='b';ptrait='--';
CAGD_tsbezier(X,Y,color,XP,YP,shift,pchar,pcolor,ptrait)
title('Bézier curve with multiple control points','FontSize',fs); hold off ;
%
% one more point to force the curve to cut
%
np=np+1;
XP=[ XP , XP(2) ] ;
YP=[ YP , YP(2) ] ;
% sampling the Bézier curve
T=[0:0.01:1.];
[X,Y]=CAGD_cbezier(T,XP,YP);
%
% graphics
%
% a) window definition 
nf=12; figure(nf) ; hold on ; fs=18;
axis([xmin,xmax,ymin,ymax]);
% b) curve display 
color='r';pchar='P';pcolor='b';ptrait='--';
CAGD_tsbezier(X,Y,color,XP,YP,shift,pchar,pcolor,ptrait)
title('Bézier curve with multiple control points','FontSize',fs); hold off ;
%
% and more points again...
%
np=np+3;
XP=[ XP , XP(3) , XP(4) , XP(5) ] ;
YP=[ YP , YP(3) , YP(4) , YP(5) ] ;
% sampling the Bézier curve
T=[0:0.01:1.];
[X,Y]=CAGD_cbezier(T,XP,YP);
%
% graphics
%
% a) window definition 
nf=13; figure(nf) ; hold on ; fs=18;
axis([xmin,xmax,ymin,ymax]);
% b) curve display 
color='r';pchar='P';pcolor='b';ptrait='--';
CAGD_tsbezier(X,Y,color,XP,YP,shift,pchar,pcolor,ptrait)
title('Bézier curve with multiple control points','FontSize',fs); hold off ;
%
% closing the curve
%
np=np+1;
XP=[ XP , XP(1) ] ;
YP=[ YP , YP(1) ] ;
% sampling the Bézier curve
T=[0:0.01:1.];
[X,Y]=CAGD_cbezier(T,XP,YP);
%
% graphics
%
% a) window definition 
nf=14; figure(nf) ; hold on ; fs=18;
axis([xmin,xmax,ymin,ymax]);
% b) curve display 
color='r';pchar='P';pcolor='b';ptrait='--';
CAGD_tsbezier(X,Y,color,XP,YP,shift,pchar,pcolor,ptrait)
title('Bézier curve with multiple control points','FontSize',fs); hold off ;
