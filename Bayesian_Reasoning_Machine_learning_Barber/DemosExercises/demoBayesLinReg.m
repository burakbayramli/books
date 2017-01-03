function demoBayesLinReg
%DEMOBAYESLINREG demo of Bayesian linear regression with hyperparameter optimisation
figure
x=[0.5+rand(1,15) 0 -0.5-rand(1,15)]; % generate some training data
N=size(x,2); % number of training points
y=sin(5*x)+0.1*randn(1,N); % training outputs

lambdavals=0.01:0.04:0.8; % basis function widths of interest
s=ceil(sqrt(length(lambdavals))); % (for plotting subplots in a square)

centres(1,:)=-3:0.1:3; % define the RBF centres of the data 
for model=1:length(lambdavals)	
	lambdas=lambdavals(model)*ones(1,size(centres,2)); % all widths are equal
	B=size(centres,2); % number of basis vectors

	% Compute the phi function statistics:
	sumPhi=zeros(B); sumyPhi=zeros(B,1);
	for n=1:N
		[dum phi]=rbf(x(n),centres,lambdas,ones(B,1)); % use the RBF
		sumPhi=sumPhi+phi*phi';
		sumyPhi=sumyPhi+y(n)*phi;
	end
	opts.maxits = 50;
	opts.tol=0.001;
	opts.plotprogress=0;
	[m S alpha beta marglik(model)]=BayesLinReg(y,sumPhi,sumyPhi,opts);
	
	testx=-2:0.01:2; % test prediction points
	for ct=1:length(testx)
		[mty(ct) phi]=rbf(testx(ct),centres,lambdas,m);
		varty(ct)=phi'*S*phi+1/beta; % predictive variance
	end

	subplot(s,s,model);hold on
	plot(testx,mty,'-r'); % mean prediction
	conf=sqrt(varty); % one standard deviation confidence
	plot(testx,mty-conf,'g'); plot(testx,mty+conf,'g'); plot(x,y,'.'); set(gca,'box','on'); 
	axis([-2 2 -2 2]); title(['lambda=',num2str(lambdavals(model))]);drawnow
end
figure; bar(lambdavals,marglik); ylabel('log marginal likelihood'); xlabel('lambda')
[val ind]=max(marglik); % find the optimal model
disp(['lambda=',num2str(lambdavals(ind)),' is optimal (according to ML-II)'])