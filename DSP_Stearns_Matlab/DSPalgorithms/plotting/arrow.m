function arrow(x1,y1,x2,y2)
% arrow(x1,y1,x2,y2)
%
% Places an arrow from (x1,y1) to (x2,y2) on an existing plot.

Lt=.2;                             %tip length (")
a=pi/6;                            %opening angle of point (rad)

%shaft of arrow goes from x1 to x2=x1+dx and y1 to y2=y1+dy
line([x1,x2],[y1,y2],'color','k');

%definitions for left to right arrow at elevation angle e
figpos=get(gcf,'position');         %left, bottom, width, height
fig_x=figpos(3);                    %x dim of fig (")
fig_y=figpos(4);                    %y dim of fig (")

axpos=get(gca,'position');          %left, bottom, width, height
ax_x=axpos(3)*fig_x;                %length of x axis (")
ax_y=axpos(4)*fig_y;                %length of y axis (")

p_limx=get(gca,'xlim');             %x limits (plot units)
p_limy=get(gca,'ylim');             %y limits (plot units)
Lxa=p_limx(2)-p_limx(1);           	%length of x axis (plot units)
Lya=p_limy(2)-p_limy(1);          	%length of y axis (plot units)

sx=ax_x/Lxa;                        %u=sx*v scales v(plot units) to u(")
sy=ax_y/Lya;                        %u=sy*v scales v(plot units) to u(")

dx=sx*(x2-x1);                      %x2-x1 (")
dy=sy*(y2-y1);                      %y2-y1 (")
e=atan2(dy,dx);                     %elevation angle (rad)
f=e+a/2;                            %elevation angle of lower branch
g=a/2-e;                            %elevation angle of upper branch

txb=x2-Lt*cos(f)/sx;                %x at bottom back of tip (plot units)
tyb=y2-Lt*sin(f)/sy;                %y at bottom back of tip (plot units)

txu=x2-Lt*cos(g)/sx;                %x at upper back of tip (plot units)
tyu=y2+Lt*sin(g)/sy;                %y at upper back of tip (plot units)

%plot of tip
line([txb,x2],[tyb,y2],'color','k');
line([txu,x2],[tyu,y2],'color','k');
