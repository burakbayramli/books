function demoGMMclass

D=2;
X1=horzcat(randn(D,10), randn(D,10)+repmat([4 4]',1,10)); X1=X1+5*ones(size(X1)); % class1 data
X2=horzcat(randn(D,10), randn(D,10)-repmat([4 4]',1,10));  % class2 data
scatter(X1(1,:),X1(2,:),'ro'); hold on; scatter(X2(1,:),X2(2,:),'b+'); set(gca,'box','on')
title('Training data in red and blue. Test point in magenta')
xtest=[X1 X2 [10 -5]']; % last point is far from the training data
figure(1); plot(xtest(1,end),xtest(2,end),'md');

pause(5)

% EM training :
opts.plotlik=1;
opts.plotsolution=1;
opts.maxit=25;
opts.minDeterminant=0.0001;
H=2; % number of mixture components

figure; [P1,m1,S1,loglik1,phgn1]=GMMem(X1,H,opts); % fit class1 data
figure; [P2,m2,S2,loglik2,phgn2]=GMMem(X2,H,opts); % fit class2 data



p1 = size(X1,2); p2=size(X2,2); % prior class probabilities
logl1=GMMloglik(xtest,P1,m1,S1);
logl2=GMMloglik(xtest,P2,m2,S2);
fprintf(1,'Class probabilities:\n the last test point which is far from the training data is over confident')
pcgdata  = condexp([logl1+log(p1); logl2+log(p2)]) % posterior probabilities
figure; subplot(2,1,1); bar(pcgdata(1,:)); axis([0 42 0 1]); title('class probabilities for the datapoints (top: standard, bottom: using an additional broad component)')

% include an additional component with large variance to ensure that classification goes to 0.5 far from data:
smallprob=0.0001;
P1=condp(vertcat(P1, smallprob));
m1=[m1 mean([X1 X2],2)];
S1(:,:,end+1)=10*cov([X1 X2]'); 

P2=condp(vertcat(P2, smallprob));
m2=[m2 mean([X1 X2],2)];
S2(:,:,end+1)=10*cov([X1 X2]');

logl1=GMMloglik(xtest,P1,m1,S1);
logl2=GMMloglik(xtest,P2,m2,S2);
fprintf(1,'\n\nClass probabilities:\n the last test point now has roughly equal class probabilities')
pcgdata2 = condexp([logl1+log(p1); logl2+log(p2)]) % posterior probabilities
subplot(2,1,2); bar(pcgdata2(1,:)); axis([0 42 0 1])