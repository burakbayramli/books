%do_ode.m
% uses quiver to plot the possible solution curve segments
% called the slope/directional field for y'(t)+y=1
clear, clf
t0=0; tf=2; tspan=[t0 tf]; x0=0;
[t,y]= meshgrid(t0:(tf-t0)/10:tf,0:.1:1);
pt=ones(size(t)); py=(1-y).*pt; %dy=(1-y)dt
subplot(221)
quiver(t,y,pt,py) %y(displacement) vs. t(time)
axis([t0 tf+.2 0 1.05]), hold on
dy=inline('-y+1','t','y');
[tR,yR]=ode_RK4(dy,tspan,x0,40);
for k=1:length(tR), plot(tR(k),yR(k),'rx'), pause(0.001); end 