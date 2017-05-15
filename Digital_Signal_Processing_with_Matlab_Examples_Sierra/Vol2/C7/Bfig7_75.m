% Stochastic process illustration
% paint confidence intervals
% for MATLAB 2007 or more

%declare anonymous function:
paint_cfi=@(t,yup,ylow,color) set(fill([t,t(end:-1:1)],[yup,ylow(end:-1:1)],color),'EdgeColor',color);

%the central function
t=0:0.4:10;
N=length(t);
y=0.5*rand(1,N);

%the confidence intervals
cfi=0.1+0.2*abs(cos(0.3*t));

%display
paint_cfi(t,y-cfi,y+cfi,'c'); hold on;
plot(t,y,'k');
axis([-0.1 10.1 -0.4 0.8]);
title('many random signals');
xlabel('t');
 