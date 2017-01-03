function [] = plot3Lines(l)
lnum = size(l,2);
for i=1:lnum
 lp(i) = line([l(1,i),l(3,i)],[l(2,i),l(4,i)]);
 set(lp(i),'Color','red');
end;


