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
%%   Intersection of two Bézier curves
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
clear all; close all;
% control points definition
ktest=1;
switch ktest
case 1
 XC1=[ 0.0 , 1.0 , 2.0 ] ;
 YC1=[ 0.0 , 1.0 , 0.0 ] ;   
 XC2=[ 0.0 ,  1.0 , 2.0 ] ; 
 YC2=[ 2.5 , -2.0 , 0.5 ] ;    
case 2
 XC1=[ -1.0 , 0.0 , 1.5 ,  2.5 , 3.0 , 2.0 ] ;
 YC1=[  0.0 , 1.0 , 1.5 , -1.0 , 0.0 , 2.5 ] ;   
 XC2=[  0.0 , 1.0 , 2.0 , 1.0 ,  0.0 ] ;
 YC2=[  2.5 , 2.25 , 1.5 , 0.25 , -0.5 ] ;     
case 3
 XC1=[ -1.0 , 1.0 , 2.0 ,  3.0 , -1.0 ] ;
 YC1=[  0.0 , 1.5 , 2.0 , -1.0 ,  0.0 ] ;   
 XC2=[  0.0 , 1.0 , 2.0 ,  1.0 ,  0.0 ] ;
 YC2=[  2.5 , 2.0 , 1.5 ,  0.5 , -0.5 ] ;   
end
%%
%%  graphics
%%
XP1=XC1;YP1=YC1;XP2=XC2;YP2=YC2;
% sampling the Bézier curves
T=[0:0.05:1.];
[X1,Y1]=CAGD_cbezier(T,XP1,YP1);
[X2,Y2]=CAGD_cbezier(T,XP2,YP2);
% setting the rectangles
[xmin1,xmax1,ymin1,ymax1]=CAGD_drectan(XP1,YP1);
[xmin2,xmax2,ymin2,ymax2]=CAGD_drectan(XP2,YP2);
dx=0.75;dy=0.75;
xmin=min(xmin1,xmin2)-dx;xmax=max(xmax1,xmax2)+dx;
ymin=min(ymin1,ymin2)-dy;ymax=max(ymax1,ymax2)+dy;
%% curves display
nf=1; figure(nf) ; hold on ;
axis([xmin,xmax,ymin,ymax]);
color='r';pchar='P';pcolor='b';ptrait='--';
CAGD_tbezier(X1,Y1,color,XP1,YP1,pchar,pcolor,ptrait);
color='g';pchar='Q';pcolor='b';ptrait='--';
CAGD_tbezier(X2,Y2,color,XP2,YP2,pchar,pcolor,ptrait);
%%
%%  first step : check intersection 
%%
[test,xmi,xma,ymi,yma]=CAGD_rbezier(XP1,YP1,XP2,YP2); 
if (test==2) 
   CAGD_trectan(xmi,xma,ymi,yma,'k'); 
else
   fprintf('\n No intersection found - Stop calculation ');
   stop
end
fs=18;title('Intersection of two curves : initial view','FontSize',fs); hold off ;
%%
%%  second step : search of intersection point
%%
nf=2; figure(nf) ; hold on ;
axis([xmin,xmax,ymin,ymax]);
color='r';pchar='P';pcolor='b';ptrait='--';
CAGD_tbezier(X1,Y1,color,XP1,YP1,pchar,pcolor,ptrait);
color='g';pchar='Q';pcolor='b';ptrait='--';
CAGD_tbezier(X2,Y2,color,XP2,YP2,pchar,pcolor,ptrait);
%% iterations
d1=0.;f1=1.;d2=0.;f2=1.;iter=0;
[XQ1,YQ1,d1,f1,XQ2,YQ2,d2,f2,iter]=CAGD_dbezier(XP1,YP1,d1,f1,XP2,YP2,d2,f2,iter);
fs=18;title('Intersection of two curves : algorithm','FontSize',fs); hold off ;
%%
%%  third step : computing the intersection point 
%%  as the common point of two segments  
%%
A=zeros(2,2);b=zeros(2,1);np1=size(XQ1,2);np2=size(XQ2,2);
A(1,1)=XQ1(np1)-XQ1(1);A(1,2)=-XQ2(np2)+XQ2(1);b(1)=XQ2(1)-XQ1(1);
A(2,1)=YQ1(np1)-YQ1(1);A(2,2)=-YQ2(np2)+YQ2(1);b(2)=YQ2(1)-YQ1(1);
delta=A(1,1)*A(2,2)-A(2,1)*A(1,2);
delt1=b(1)*A(2,2)-b(2)*A(1,2);
delt2=A(1,1)*b(2)-A(2,1)*b(1);
c(1)=delt1/delta;c(2)=delt2/delta;
XI1=XQ1(1)+c(1)*(XQ1(np1)-XQ1(1));YI1=YQ1(1)+c(1)*(YQ1(np1)-YQ1(1));
XI2=XQ2(1)+c(2)*(XQ2(np2)-XQ2(1));YI2=YQ2(1)+c(2)*(YQ2(np2)-YQ2(1));
fprintf('\n Intersection point %8.5f %8.5f %8.5f %8.5f ',XI1,YI1,XI2,YI2);
%%
%%  fourth step : graphical check
%%
nf=3; figure(nf) ; hold on ; 
[test,xmi,xma,ymi,yma]=CAGD_rbezier(XQ1,YQ1,XQ2,YQ2); 
dx=(xma-xmi)*0.1;dy=(yma-ymi)*0.1;
axis([xmi-dx,xma+dx,ymi-dy,yma+dy]);
T=[0:0.1:1.];
[X1,Y1]=CAGD_cbezier(T,XQ1,YQ1);
[X2,Y2]=CAGD_cbezier(T,XQ2,YQ2);
color='r';pchar='P';pcolor='b';ptrait='--';
CAGD_tbezier(X1,Y1,color,XQ1,YQ1,pchar,pcolor,ptrait);
color='g';pchar='Q';pcolor='b';ptrait='--';
CAGD_tbezier(X2,Y2,color,XQ2,YQ2,pchar,pcolor,ptrait);
%%  View of intersection point
text(XI1,YI1,'X');text(XI2,YI2,'O');
fs=18;title('Intersection of two curves : local view','FontSize',fs); hold off ;
%%
%%  fifth step : computing the parameter values
%%
fprintf('\n Intersection of two curves : local formula');
tx1=(XI1-XQ1(1))/(XQ1(np1)-XQ1(1));ty1=(YI1-YQ1(1))/(YQ1(np1)-YQ1(1));
[XXI1,YYI1]=CAGD_casteljau(tx1,XQ1,YQ1);[XXXI1,YYYI1]=CAGD_casteljau(ty1,XQ1,YQ1);
fprintf('\n Values for curve n 1 (computed with x) : %8.5f %8.5f %8.5f %8.5f %8.5f ',tx1,XI1,YI1,XXI1,YYI1);
fprintf('\n Values for curve n 1 (computed with y) : %8.5f %8.5f %8.5f %8.5f %8.5f ',ty1,XI1,YI1,XXXI1,YYYI1);
tx2=(XI2-XQ2(1))/(XQ2(np2)-XQ2(1));ty2=(YI2-YQ2(1))/(YQ2(np2)-YQ2(1));
[XXI2,YYI2]=CAGD_casteljau(tx2,XQ2,YQ2);[XXXI2,YYYI2]=CAGD_casteljau(ty2,XQ2,YQ2);
fprintf('\n Values for curve n 2 (computed with x) : %8.5f %8.5f %8.5f %8.5f %8.5f ',tx2,XI2,YI2,XXI2,YYI2);
fprintf('\n Values for curve n 2 (computed with y) : %8.5f %8.5f %8.5f %8.5f %8.5f ',ty2,XI2,YI2,XXXI2,YYYI2);
%%
%%  sixth step : global view
%%
%% Bézier curves display 
T=[0:0.05:1.];[X1,Y1]=CAGD_cbezier(T,XP1,YP1);[X2,Y2]=CAGD_cbezier(T,XP2,YP2);
nf=4; figure(nf) ; hold on ;
[test,xmi,xma,ymi,yma]=CAGD_rbezier(XP1,YP1,XP2,YP2);
axis([xmin,xmax,ymin,ymax]);
color='r';pchar='P';pcolor='b';ptrait='--';
CAGD_tbezier(X1,Y1,color,XP1,YP1,pchar,pcolor,ptrait);
color='g';pchar='Q';pcolor='b';ptrait='--';
CAGD_tbezier(X2,Y2,color,XP2,YP2,pchar,pcolor,ptrait);
xx=(XXI1+XXI2)*0.5;yy=(YYI1+YYI2)*0.5;
dx=(xmax-xmin)/100;dy=(ymax-ymin)/100;
CAGD_trectan(xx-dx,xx+dx,yy-dy,yy+dy,'r'); 
fs=18;title('Intersection of two curves : global view','FontSize',fs); hold off ;
%%
%%  seventh step : computing the parameter values
%%
fprintf('\n Intersection : global formula ');
T=[0:0.02:1.];[X1,Y1]=CAGD_cbezier(T,XP1,YP1);[X2,Y2]=CAGD_cbezier(T,XP2,YP2);
[xmin1,xmax1,ymin1,ymax1]=CAGD_drectan(XP1,YP1);
[xmin2,xmax2,ymin2,ymax2]=CAGD_drectan(XP2,YP2);
seuilx=1.e-03;seuily=1.e-03;test1=0;test2=0;
for k=1:size(T,2)-1
if abs((X1(k)-XI1)*(XI1-X1(k+1))) < seuilx * (xmax1-xmin1) * (xmax1-xmin1)
if abs((Y1(k)-YI1)*(YI1-Y1(k+1))) < seuily * (ymax1-ymin1) * (ymax1-ymin1)
tx1=T(1,k+1)*(XI1-X1(k))/(X1(k+1)-X1(k))+T(1,k)*(X1(k+1)-XI1)/(X1(k+1)-X1(k));
ty1=T(1,k+1)*(YI1-Y1(k))/(Y1(k+1)-Y1(k))+T(1,k)*(Y1(k+1)-YI1)/(Y1(k+1)-Y1(k));
test1=1;%% fprintf('\n Intersection point on curve n 1 : %8.5f %8.5f %d ',tx1,ty1,k);
end;end;end;
%% fprintf('\n Check position on curve n 1 : %d ',test1);
for k=1:size(T,2)-1
if abs((X2(k)-XI2)*(XI2-X2(k+1))) < seuilx * (xmax2-xmin2) * (xmax2-xmin2)
if abs((Y2(k)-YI2)*(YI2-Y2(k+1))) < seuily * (ymax2-ymin2) * (ymax2-ymin2)
tx2=T(1,k+1)*(XI2-X2(k))/(X2(k+1)-X2(k))+T(1,k)*(X2(k+1)-XI2)/(X2(k+1)-X2(k));
ty2=T(1,k+1)*(YI2-Y2(k))/(Y2(k+1)-Y2(k))+T(1,k)*(Y2(k+1)-YI2)/(Y2(k+1)-Y2(k));
test2=1;%% fprintf('\n Intersection point on curve n 2 : %8.5f %8.5f %d ',tx2,ty2,k);
end;end;end;
%% fprintf('\n Check position on curve n 2 : %d ',test2);
[XXI1x,YYI1x]=CAGD_casteljau(tx1,XP1,YP1);[XXI1y,YYI1y]=CAGD_casteljau(ty1,XP1,YP1);
fprintf('\n Intersection point on curve n 1 (computed with x) : %8.5f %8.5f %8.5f %8.5f %8.5f ',tx1,XI1,YI1,XXI1x,YYI1x);
fprintf('\n Intersection point on curve n 1 (computed with y) : %8.5f %8.5f %8.5f %8.5f %8.5f ',ty1,XI1,YI1,XXI1y,YYI1y);
[XXI2x,YYI2x]=CAGD_casteljau(tx2,XP2,YP2);[XXI2y,YYI2y]=CAGD_casteljau(ty2,XP2,YP2);
fprintf('\n Intersection point on curve n 2 (computed with x) : %8.5f %8.5f %8.5f %8.5f %8.5f ',tx2,XI2,YI2,XXI2x,YYI2x);
fprintf('\n Intersection point on curve n 2 (computed with y) : %8.5f %8.5f %8.5f %8.5f %8.5f ',ty2,XI2,YI2,XXI2y,YYI2y);
nf=5; figure(nf) ; hold on ;
[test,xmi,xma,ymi,yma]=CAGD_rbezier(XP1,YP1,XP2,YP2);
axis([xmin,xmax,ymin,ymax]);
color='r';pchar='P';pcolor='b';ptrait='--';
CAGD_tbezier(X1,Y1,color,XP1,YP1,pchar,pcolor,ptrait);
color='g';pchar='Q';pcolor='b';ptrait='--';
CAGD_tbezier(X2,Y2,color,XP2,YP2,pchar,pcolor,ptrait);
dx=(xmax-xmin)/100;dy=(ymax-ymin)/100;
XCr(1)=XXI1x-dx;XCr(2)=XXI1x+dx;YCr(1)=YYI1x;YCr(2)=YYI1x;plot(XCr,YCr,'r');
XCr(1)=XXI1x;XCr(2)=XXI1x;YCr(1)=YYI1x-dy;YCr(2)=YYI1x+dy;plot(XCr,YCr,'r');
XCr(1)=XXI2x-dx;XCr(2)=XXI2x+dx;YCr(1)=YYI2x-dy;YCr(2)=YYI2x+dy;plot(XCr,YCr,'b');
XCr(1)=XXI2x-dx;XCr(2)=XXI2x+dx;YCr(1)=YYI2x+dy;YCr(2)=YYI2x-dy;plot(XCr,YCr,'b');
fs=18;title('Intersection of two curves : global view','FontSize',fs); hold off ;