function [] = plot3Lines(l)

lnum = size(l,2);
for i=1:lnum
% l(1:3,:) is the first point , l(4:6,:) the second
 lp(i) = line([l(1,i),l(4,i)],[l(2,i),l(5,i)],[l(3,i),l(6,i)]);
end;


