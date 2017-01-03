function demoBayesErrorAnalysis
%DEMOBAYESERRORANALYSIS demo of Bayesian error analysis
N = 20; % number of datapoints
Q = 3; % number of error types

% make some errors for the two classifers :
errtype={'IndepSameErrorGenerator','IndepDiffErrorGenerator','DepErrorGenerator'};
GenerateError=errtype{randgen(1:3)}
switch GenerateError
	case 'IndepDiffErrorGenerator'	% independent different error generator:
		ra = rand(Q,1); rb = rand(Q,1);
		rab = ra*rb';
		relprobC = vec(rab);

	case 'IndepSameErrorGenerator'% independent same error generator:
		ra = rand(Q,1); rb = ra;
		rab = ra*rb';
		relprobC = vec(rab);

	case 'DepErrorGenerator'% dependent error generator:
		relprobC = vec(rand(Q,Q));
end
errors = randgen(relprobC,1,N); % sample the errors

% form the count matrices and vectors:
vec_errorAB=zeros(1,Q*Q);
for i=1:N
	vec_errorAB(errors(i))= vec_errorAB(errors(i))+1;
end
C = vec2mat(vec_errorAB)  % count matrix of errors
ca = sum(C,1); cb = sum(C,2)';

% Simple Bayesian analysis of the error counts based on Dirichlet
% prior and multinomial error distribution:

u =ones(1,Q); % uniform prior distribution
lb = logZ(u+ca)+logZ(u+cb) - logZ(u)- logZ(u+ca+cb);
disp(['p(classifiers are indep and different)/p(classifiers are independent and same)=',num2str(exp(lb))]);

U=ones(Q,Q); % uniform prior
lb = logZ(u+ca)+logZ(u+cb)+logZ(vec(U))-2*logZ(u)-logZ(vec(U+C));
disp(['p(classifiers are indep and different)/p(classifiers are dependent)=',num2str(exp(lb))])

% test Hsame/Hdep
lb = logZ(u+ca+cb)+logZ(vec(U))-logZ(u)-logZ(vec(U+C));
disp(['p(classifiers are indep and same)/p(classifiers are dependent)=',num2str(exp(lb))]);

function v = vec(C)
tmp=C'; v=tmp(:);

function C=vec2mat(v)
Q = sqrt(length(v));  C= reshape(v,Q,Q)';

function lz=logZ(u)
lz = sum(gammaln(u))- gammaln(sum(u));