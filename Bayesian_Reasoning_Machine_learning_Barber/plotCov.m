function x=plotCov(m,S,L)
%PLOTCOV return points for plotting an ellipse of a covariance
% x=plotCov(m,S,L)
% m : mean
% S : covariance
% L : length scale
[E V]=eig(S);
dV=L*sqrt(diag(V));
theta=0:0.05:2*pi+0.5;
p(1,:)= dV(1)*cos(theta);
p(2,:)= dV(2)*sin(theta);
x = E*p+repmat(m,1,length(theta));
plot(x(1,:),x(2,:),':');