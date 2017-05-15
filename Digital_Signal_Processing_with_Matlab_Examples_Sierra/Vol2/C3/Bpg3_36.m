% Sinogram for one point
%the point
x0=5; y0=4;

%sinogram preparation
NP=360; %number of points
alpha=1:1:NP; 
alpha=alpha.*pi/180; %angle in rads
P=zeros(1,NP); %space for projections

for nn=1:NP,
   P(nn)=(x0*cos(alpha(nn)))+(y0*sin(alpha(nn)));
end;

figure(1)
plot(alpha(:),P(:),'k');
axis([0 2*pi 1.2*min(P) 1.2*max(P)]);
title('sinogram for one point');
xlabel('angle in degrees');
ylabel('Projection');
   