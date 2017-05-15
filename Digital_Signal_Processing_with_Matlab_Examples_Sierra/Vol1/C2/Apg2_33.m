% Monte Carlo points, and area approximation
%the curve
x=0:0.1:10;
y=0.5+(0.3*sin(0.8*x));

%the random points
N=500; %number of points
px=10*rand(1,N); %uniforma distribution
py=rand(1,N); % "   "   "

plot(x,y,'k'); hold on;
plot(px,py,'b.');
axis([0 10 0 1]);
xlabel('x');ylabel('y');
title('curve and random points');

%area calculation
na=0; %counter of accepted points
for nn=1:N,
   xnn=px(nn); ynn=0.5+(0.3*sin(0.8*xnn));
   if py(nn)<ynn, na=na+1; end; %point accepted
end;

%print computed area 
%the plot rectangle area is 10
A=(10*na)/N
