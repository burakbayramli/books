function demoMetropolis
%DEMOMETROPOLIS demo of Metropolis sampling
figure
yj=0; xi=0; xxx=-6:0.1:6; yyy=-6:0.1:6;
for yy=yyy
	yj=yj+1; xi=0;
	for xx=xxx
		xi=xi+1; z(xi,yj)=exp(logp([xx yy]));
	end
end
contour(xxx,yyy,z,20);  hold on; % iso-probability contours
axis image

% Do Metropolis sampling:
L=2000; % number of samples
x(:,1)=randn(2,1); % intial sample
s=1; % width of Metropolis candidate distribution
for l=2:L
	x(:,l)=metropolis(x(:,l-1),s,'logp');
	plot(x(1,l),x(2,l),'.');
	if mod(l,100)==1; drawnow; end % plot every 100 samples
end