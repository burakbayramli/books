%BMEAN    Sample mean of function along a Brownian path
%         Function is exp(t + 0.5*W(t))

clf
randn('state',100)                                % set the state of randn
T = 1; N = 500; dt = T/N; t = [dt:dt:1];

M = 200;                                          % M paths simultaneously
dW = sqrt(dt)*randn(M,N);                         % increments
W = cumsum(dW,2);                                 % cumulative sum
U = exp(repmat(t,[M 1]) + 0.5*W);
Umean = mean(U);
Ustd = std(U);
Upper = Umean + 1.96*Ustd/sqrt(M);
Lower = Umean - 1.96*Ustd/sqrt(M);

True = exp(9*t/8); 

plot([0,t],[1,Umean],'b-','LineWidth',2), hold on  % plot mean over M paths
plot([0,t],[1,Upper],'g-','LineWidth',2)           % plot max of conf. int.
plot([0,t],[1,Lower],'c-','LineWidth',2)           % plot min of conf. int.
plot([0,t],[1,True],'r-','LineWidth',2)            % plot true mean value

xlabel('t')
legend('mean of 200 paths','max of conf. int.','min of conf. int.',...
             'true mean','Location','NorthWest')


