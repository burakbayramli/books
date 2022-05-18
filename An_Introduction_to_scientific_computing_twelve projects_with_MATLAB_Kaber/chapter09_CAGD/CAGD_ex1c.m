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
%%   Display of the control polygon
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Example of a non convex control polygon 
%%
clear all; close all;
% control points definition 
np=5;
XP=zeros(np,1);YP=zeros(np,1);
XP=[ 0. , 3. , 2. , 1. , 3.5 ] ;
YP=[ 0. , 1.5 , 3. , 2.5 , 0. ] ;
% sampling the Bézier curve
T=[0:0.05:1.];
[X,Y]=CAGD_cbezier(T,XP,YP);
%
% graphics
%
% a) window definition 
nf=1; figure(nf) ; hold on ; fs=18;
xmin=min(XP)-0.5;xmax=max(XP)+0.5;
ymin=min(YP)-0.5;ymax=max(YP)+0.5;
axis([xmin,xmax,ymin,ymax]);
% b) curve display 
color='r';pchar='P';pcolor='b';ptrait='--';
CAGD_pbezier(X,Y,XP,YP,color,pchar,pcolor,ptrait)
title('Bézier curve with control polygon','FontSize',fs); hold off ;
