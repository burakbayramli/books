function lc_image
% LC_IMAGE
%
% This script makes the figure in Chapter 3 which illustrates
% stagnation in the case where the stencil directions and an
% explict constraint combine to hide all descent directions.
%
v0=[0,0]';
vx=[0, 1]';
vy=[1,0]';
x0=[.5,.5]';
v=[0 1; 0 -1; 1 0; -1 0]';
h=.25;
xyb=[.5-h  .5  .5+h]';
sb=[.5 .5 .5]';
xnew=[.5-h,.5]'; ynew=[.5+.5*h, .5]';
p1=subplot(1,1,1);
p2=plot(vx,vy,'-',v0,vy,'-',vx,v0,'-',sb,xyb,'-o',xyb,sb,'-o',xnew,ynew,'-o');
ktext(.45,.45,'x_0');
ktext(.7,.55,'x_{right}');
ktext(.15,.5,'x_{left}');
ktext(.53,.75,'x_{up}');
ktext(.5,.2,'x_{down}');
ktext(.15,.6,'x_{new}');
ktext(.45,.95,'Infeasible Region');
set(p1,'FontSize',12,'FontWeight','Bold','YTick',[0:.2:1],'XTick',[0:.2:1]);
set(p2,'LineWidth',1.5,'Color','black');
axis('square');
hold on
nshade=10;
dshade=1/nshade;
for i=1:nshade-1
wx=[1-dshade*i,1]';
wy=dshade*[i,i]';
qx=[dshade*i,dshade*i];
qy=[1-dshade*i,1]';
if i > 4 & i < 9
   qy(2)=.9;
end
p3=plot(wx,wy,'--',qx,qy,'--');
set(p3,'LineWidth',.5,'Color','black');
end
hold off


function ktext(x,y,words)
text(x,y,words,'FontSize',12,'LineWidth',1,'FontWeight','Bold');
