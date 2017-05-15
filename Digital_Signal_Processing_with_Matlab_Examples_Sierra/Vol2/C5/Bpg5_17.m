% SVD axes example

A=[1 1.5; 2 2]; %the A matrix

%a circle
R=1; %radius
fi=0:0.01:(2*pi);%angle
x1=R*cos(fi); x2=R*sin(fi);
x=[x1;x2];

%the image of the circle, using A
px=A*x;

%the SVD decomposition
[u,s,v]=svd(A);

%print diagonal of s (the singular values)
diag(s)

%print condition(A)
cond(A)

%display
figure(1)
subplot(1,2,1)
plot(x(1,:),x(2,:),'k');
title('the original circle');

subplot(1,2,2)
plot(px(1,:),px(2,:),'k'); hold on;
L=s(1,1); %the first singular value
plot([-L*u(1,1) L*u(1,1)],[-L*u(2,1) L*u(2,1)],'g');
L=s(2,2); %the second singular value
plot([-L*u(1,2) L*u(1,2)],[-L*u(2,2) L*u(2,2)],'g');
title('the A image of the circle');

