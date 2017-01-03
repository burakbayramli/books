function demoNearNeigh
%DEMONEARNEIGH demo of Nearest Neighbour classification
n=[10 20]; % number of traning points in classes

xtrain{1}=randn(2,n(1));  % class 1 training data
trainlabel{1}=ones(1,n(1));

xtrain{2}=randn(2,n(2))+repmat([3 3]',1,n(2)); % class 2 training data
trainlabel{2}= 2*ones(1,n(2)); 

xtest=3*randn(2,30)+repmat([1.5 1.5]',1,30); % test datapoints

y = nearNeigh([xtrain{1} xtrain{2}], xtest, [trainlabel{1} trainlabel{2}],3); % 3 nearest neighbours

figure; hold on;
plot(xtrain{1}(1,:), xtrain{1}(2,:),'ro'); 
plot(xtrain{2}(1,:), xtrain{2}(2,:),'b+');

for i=1:length(y)
	if y(i)==1; 
		plot(xtest(1,i), xtest(2,i),'rx');
	else
		plot(xtest(1,i), xtest(2,i),'bx');
	end
end