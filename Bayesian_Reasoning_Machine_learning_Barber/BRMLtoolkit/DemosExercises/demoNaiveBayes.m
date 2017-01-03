function demoNaiveBayes
%DEMONAIVEBAYES Naive Bayes using Bernoulli Distribution
xE=[0 1 1 1 0 0;  % English
    0 0 1 1 1 0;
    1 1 0 0 0 0;
    1 1 0 0 0 1;
    1 0 1 0 1 0];

xS=[1 1 1 1 1 1 1; % Scottish
    0 1 1 1 1 0 0;
    0 0 1 0 0 1 1;
    1 0 1 1 1 1 0;
    1 1 0 0 1 0 0];

pE = size(xE,2)/(size(xE,2) + size(xS,2)); pS =1-pE; % ML class priors pE = p(c=E), pS=p(c=S)

mE = mean(xE')'; % ML estimates of p(x=1|c=E)
mS = mean(xS')'; % ML estimates of p(x=1|c=S)

xtest=[1 0 1 1 0]'; % test point

npE = pE*prod(mE.^xtest.*(1-mE).^(1-xtest)); % p(x,c=E)
npS = pS*prod(mS.^xtest.*(1-mS).^(1-xtest)); % p(x,c=S)

pxE = npE/(npE+npS); % probability that x is English
disp(['probability x is English = ',num2str(pxE)]);

% Repeat with a Dirichlet prior on the tables: 
uprior=ones(2,5,2); % uniform prior
x{1}=xE+1; x{2}=xS+1; % add 1 to using (1,2) class encoding
[classML upost]=NaiveBayesDirichletTrain(x,uprior);
classpost=NaiveBayesDirichletTest(xtest+1,classML,upost);
disp(['probability x is English (using Dirichlet)= ',num2str(classpost(1))]);