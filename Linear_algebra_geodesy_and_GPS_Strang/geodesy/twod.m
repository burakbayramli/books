function twod(c)
%TWOC Plot of 100 random positions and a
%      confidence circle of radius c

%Kai Borre 01-01-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

if nargin == 0
   c = 1;
end
if c > 4
   c = 4;
   disp('c was set to 4'); 
end
x = randn(100,1);
y = randn(100,1);

theta = pi*[0:.05:2];
cx = c*cos(theta);
cy = c*sin(theta);

figure;
hold on
axis([-4 4 -4 4]);
axis('square');
pl = plot(x,y,'b+',cx,cy,'r');
hold off
set(gca,'FontSize',24);
print twod -deps
%%%%%%%%% end twod.m %%%%%%%%%%%%%%%%
