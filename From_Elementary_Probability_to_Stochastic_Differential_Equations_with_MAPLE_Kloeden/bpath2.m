%BPATH2  Brownian path simulation: vectorized 

randn('state',100)          % set the state of randn
T = 1; N = 500; dt = T/N;

dW = sqrt(dt)*randn(1,N);   % increments
W = cumsum(dW);             % cumulative sum

plot([0:dt:T],[0,W],'r-')   % plot W against t
xlabel('t','FontSize',16)
ylabel('W(t)','FontSize',16,'Rotation',0)
