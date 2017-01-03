function demoKmeans
%DEMOKMEANS K-means clustering demo
x = [randn(2,150) 5+randn(2,150) (repmat([-4 4]',1,250)+randn(2,250))]; % datapoints

opts.plotprogress=1;
opts.tol=0.00001;
opts.maxit=100;
[m,mindist]=Kmeans(x,3,opts);

figure; plot(x(1,:),x(2,:),'.'); hold on;
plot(m(1,:),m(2,:),'rx','markersize',10,'linewidth',2);