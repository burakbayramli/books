function demoPolya
for K=[50 100 500]; % number of clusters
	N=50; % number of datapoints
	S=500; % number of samples
	alpha=2.0;
	for s=1:S
		pis=dirrnd(alpha/K*ones(1,K),1); % sample a random Dirichlet distribution
		for n=1:N
			z(n)=randgen(pis); % sample from the multinomial
		end
		uniqueclusters(s) = length(unique(z)); % find the number of clusters
	end
	figure;hist(uniqueclusters,1:N); title(['number of clusters=',num2str(K)]); 
end