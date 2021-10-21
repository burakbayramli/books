function speed = speedlinear(density,X)
% constant speed in each zone
global umax umax1

for i=1:length(X)
   if X(i) < 0
       speed(i) = umax;
     else
       speed(i) = umax1;
     end
   end
