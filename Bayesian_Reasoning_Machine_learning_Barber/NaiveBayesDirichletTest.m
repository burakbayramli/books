function classpost=NaiveBayesDirichletTest(xtest,classML,upost)
%NAIVEBAYESDIRICHLETTEST Naive Bayes prediction having used a Dirichlet prior for training
% classpost=NaiveBayesDirichletTest(xtest,classML,upost)
% 
% Inputs:
% xtest : contains the data
% classML : a vector of max likelihood probabilities for each class
% upost : the parameters of the Dirichlet postrior
% See also NaiveBayesDirichletTrain.m, demoNaiveBayes.m
[S D C]=size(upost);
for c=1:C
	logclasspost(c,1)=log(classML(c));
	for i=1:D
		for s=1:S
			utest(s,i,c) = upost(s,i,c)+ (xtest(i)==s);
		end
		logclasspost(c) = logclasspost(c) + logZdirichlet(utest(:,i,c))-logZdirichlet(upost(:,i,c));
	end
end
classpost=condexp(logclasspost);