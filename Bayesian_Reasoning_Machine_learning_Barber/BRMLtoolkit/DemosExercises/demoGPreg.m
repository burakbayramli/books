function demoGPreg
%DEMOGPREG demo of Gaussian Process regression
figure

par(1).value=1; % prior variance of the clean output
par(2).value=2; % inverse scale
par(3).value=2; % gamma for the Gamma-Exponential Covariance
%par(4).value=0.0001; % jitter
yvar=0.0001; % variance of the training outputs
covfn='covfnGE'; % Gamma-Exponential Covariance Function

% try to understand the GP function prior by drawing sample functions
x=-10:0.05:10; P=size(x,2);
K = feval(covfn,x,x,par); % Covariance function
y=mvrandn(zeros(P,1),K,5);
plot(x,y);

% make some training data:
xtrain=[(-0.75 + rand(1,20)) (0.75+ rand(1,20))]; N=size(xtrain,2);
ytrain = sin(4*xtrain)+ 0.1*randn(1,N);

xtest=[-4:0.1:4];
[meantest,vartest,logpygx]=GPreg(xtrain,ytrain,xtest,par,covfn,yvar);

figure
plot(xtest,meantest,'r-');  hold on; % mean prediction
plot(xtest,meantest-sqrt(vartest),'g-'); % standard error bars (on predicting clean output)
plot(xtest,meantest+sqrt(vartest),'g-');
plot(xtrain,ytrain,'.');