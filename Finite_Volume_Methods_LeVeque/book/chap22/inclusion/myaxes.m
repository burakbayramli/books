function myaxes(v)
% makes axes which have the proper aspect ratio when printed

x1=v(1);  x2=v(2);  y1=v(3);  y2=v(4);
xlength = 0.8
yheight = (y2-y1)/(x2-x1) * xlength * (16.2/12.2);
if yheight > 0.8
  xlength = xlength * 0.8 / yheight;
  yheight = 0.8;
  end

clf
axes('position',[.1 .1 xlength yheight]);
axis([x1 x2 y1 y2])

