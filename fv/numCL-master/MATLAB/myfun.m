function [f,fp] = myfun(x_past,x,t,b)

f = x - x_past - (b*sin(x_past)).*t;
fp = -1 - t.*b.*cos(x_past);

return