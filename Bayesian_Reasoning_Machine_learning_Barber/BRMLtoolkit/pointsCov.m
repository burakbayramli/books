function x=pointsCov(m,S)
%POINTSCOV Points defining the unit variance contours of a 2D Gaussian with mean m and covariance S
% x=pointsCov(m,S)
[E V]=eig(S);dV=sqrt(diag(V)); 
theta=0:0.3:2*pi;
p(1,:)= dV(1)*cos(theta); p(2,:)= dV(2)*sin(theta);
x = E*p+repmat(m,1,length(theta));