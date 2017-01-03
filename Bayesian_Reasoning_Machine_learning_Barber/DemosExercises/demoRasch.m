function demoRasch
%DEMORASCH demo of training a Rasch questionaire model
Q=20; % number of questions
S=50; % number of students

% sample some results to make some training data
a_true=randn(1,S);
d_true=randn(Q,1);
X = real(sigma(repmat(a_true,Q,1) - repmat(d_true,1,S))>rand(Q,S));
imagesc(X); xlabel('student'); ylabel('question'); colormap bone
figure
bar(mynansum(X)/Q); xlim([0 S+1]); xlabel('student'); ylabel('fraction of questions correct'); 

figure
opts.maxits=100; opts.plotprogress=1;
[a d]=rasch(X,opts);

figure
subplot(1,2,1); bar(a_true); title('true ability'); xlim([0 S+1])
subplot(1,2,2); bar(a); title('estimated ability'); xlim([0 S+1])

figure
subplot(1,2,1); bar(d_true); title('true difficulty'); xlim([0 Q+1])
subplot(1,2,2); bar(d); title('estimated difficulty'); xlim([0 Q+1])