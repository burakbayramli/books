function [classML upost]=NaiveBayesDirichletTrain(x,uprior)
%NAIVEBAYESDIRICHLETTRAIN Naive Bayes training using a Dirichlet prior
% [classprior upost]=NaiveBayesDirichletTrain(x,uprior)
% 
% Inputs:
% x{c} contains the data for class c. Each column of this matrix is a datapoint
% uprior(s,i,c) is the Dirichlet prior vector for each s (state 1,2,3,...), i (component of x) and c (datapoint class)
%
% Outputs:
% classprior : p(c|data)
% upost : Dirichlet posterior parameters
% See also demoNaiveBayes.m, NaiveBayesDirichletTest
[S D C]=size(uprior); 
upost=zeros(S,D,C);
for c=1:C
	n(c)=size(x{c},2);
	for s=1:S
		for i=1:D
			upost(s,i,c) = uprior(s,i,c)+ sum(x{c}(i,:)==s);
		end
	end
end
classML=condp(n(:)); % maximum likelihood setting