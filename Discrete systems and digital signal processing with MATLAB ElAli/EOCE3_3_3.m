% We first generate points in time for plotting
t=[-pi:.05:pi];
size_of_t=size(t);
F=(pi/4)*ones(size_of_t); % for Fourier series
for n=1:6 % considering 6 points
F=F+(1/pi)*(-2*cos((2*n-1)*t)/(2*n-1)^2)-((-1)^n*sin(n*t)/n);
end
% now we create x(t)
xplot=zeros(size_of_t);
for k=1:length(t)
	if t(k) < 0
	xplot(k)=0;
	else
	xplot(k)=t(k);
end
end
subplot(2,1,1),plot(t,F,t,xplot);
xlabel('t')
ylabel('x(t)')
title('Approximation to x(t), n=6')
% we can add more point: add 15 more
for n=7:21
F=F+(1/pi)*(-2*cos((2*n-1)*t)/(2*n-1)^2)-((-1)^n*sin(n*t)/n);
end
subplot(2,1,2),plot(t,F,t,xplot);
xlabel('t'),
ylabel('x(t)'),
title('Approximation to x(t), n=21');
