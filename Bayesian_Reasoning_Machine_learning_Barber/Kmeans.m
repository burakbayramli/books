function [m,mindist,quant]=Kmeans(x,K,opts)
%KMEANS K-means clustering algorithm
% [m,mindist,quant]=Kmeans(x,K,opts)
%
% x: data matrix. Each column contains a datapoint
% K : number of components
% opts.maxit
% opts.tol
% opts.plotprogress
%
% Outputs:
% m : means (component centres)
% mindist : mean distance from centres to nearest datapoint
% quant : the vector quantisation of each point in x (one of the centres)
% See also demoKmeans.m

%r = randperm(size(x,2));
%m(:,1:K) = x(:,r(1:K)); % initialise the clusters to K randomly chosen datapoints
m=repmat(mean(x')',1,K); m=m+0.001*randn(size(m)); % initalise means to the perturbed data mean

mold =m;
for iteration = 1:opts.maxit % maximum number of iterations
	d=sqdist(m,x);    % calculate the distances between centres and datapoints
	[a,b]=min(d);     % find the nearest centres
	for k = 1:K
		if length(find(b==k))>0
			m(:,k) = mean(x(:,find(b==k))')';
		end
	end
	mdist(iteration)=mean(a);
	if opts.plotprogress; plot(mdist,'-o'); title('mean square distance to nearest centre');  drawnow; end
	if mean(sum( (m-mold).^2)) < opts.tol; break; end; mold =m; % termination criterion
end
mindist=mdist(end);
quant=m(:,b);