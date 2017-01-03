function demoLogReg
% get some training data
disp('input 5 training points for class 1 (use the mouse) and then 5 for class 0')
N = 5; % number of training points (for each class) 
[in10,in20]=ginput(N);
x0 = [in10 in20]';
plot(in10,in20,'ko'); axis([0 1 0 1]); set(gca,'box','on');

hold on
[in11,in21]=ginput(N);
x1 = [in11 in21]'; plot(in11,in21,'ko','MarkerFaceColor','k'); drawnow

opts.maxit=100; 
opts.tol=0.00001;
opts.eta=0.5;
[w b p10 p11]=LogReg(x0,x1,opts); % train using Gradient Ascent

% plot the decision boundary
xx1=-0:0.1:1; xx2 = -(b+w(1)*xx1)./w(2); plot(xx1,xx2)

% confidence intervals 0.1 and 0.9
xx2 = (1.8 -b-w(1)*xx1)./w(2); plot(xx1,xx2,'--')
xx2 = (-1.8 -b-w(1)*xx1)./w(2); plot(xx1,xx2,'--')