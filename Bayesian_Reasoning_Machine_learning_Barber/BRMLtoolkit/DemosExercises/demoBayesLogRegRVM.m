function demoBayesLogRegRVM
%DEMOBAYESLOGREGRVM demo of fitting a Relevance Vector Machine
figure
% first make some training data
tmp=15; x=randn(2,tmp); 
x=[x (randn(2,tmp)+2*ones(2,tmp))]; % class 0
x=[x (randn(2,tmp)-2*ones(2,tmp))]; % class 0
c(tmp+1:3*tmp)=1; c(1:tmp)=0; % class labels
N=3*tmp; % total number of training points

% plot the training data:
figure; hold on
for n=1:N
	if c(n)==0
		plot(x(1,n),x(2,n),'ro','markersize',5);
	else
		plot(x(1,n),x(2,n),'gd','markersize',5);
	end
end
axis([-5 5 -5 5]); set(gca,'box','on');

% define the basis matrix: place one on a subset of datapoints
pref=1; lambda=2; selfvar=0.000001;
r=randperm(N);
for m=1:N
	for n=1:N
		phi(m,n)=kernel(x(:,r(m)),x(:,n),pref,lambda,selfvar);
	end
end
M=size(phi,1);

opts.HypIterations=150;
opts.NewtonIterations=10;
opts.plotprogess=1;
for HypUpdate=1:2
	opts.HypUpdate=HypUpdate; % try both EM and Mackay/Gull
	figure
	[w,Smat,alpha,loglik]=BayesLogRegressionRVM(phi,c,zeros(M,1),1*ones(M,1),opts);
	loglik
end
% compute training classifications
for n=1:N; ptrain(n) = avsigmaGauss(w'*phi(:,n),phi(:,n)'*Smat*phi(:,n)); end

figure; hold on
for n=1:N
	if c(n)==0
		plot(x(1,n),x(2,n),'ro','markersize',5);
	else
		plot(x(1,n),x(2,n),'gd','markersize',5);
	end
	if ptrain(n)<0.5
		plot(x(1,n),x(2,n),'ro','markersize',8);
	else
		plot(x(1,n),x(2,n),'gd','markersize',8);
	end
end
% plot probability contours (very course grid -- beware interpreting plot too literally!)
xx=-5:0.5:5; yy=-5:0.5:5;
for i=1:length(xx)
	for j=1:length(yy)
		xtestpoint=[xx(i) yy(j)]';
		for m=1:M
			k(m,1)=kernel(x(:,r(m)),xtestpoint,pref,lambda,selfvar);
		end
		z(i,j)=avsigmaGauss(w'*k,k'*Smat*k);
	end
end
[cs,hh]=contour(xx,yy,z);
clabel(cs,hh,'fontsize',10); axis([-5 5 -5 5])