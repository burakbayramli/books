function demoGibbsGauss
%DEMOGIBBSGAUSS demo of Gibbs sampling of a bivariate Gaussian
figure
A=randn(2,2); S = A*A'; m=zeros(2,1);
plotCov(m,S,2); axis equal; hold on
x(:,1) = zeros(2,1);
l=1;
for count=1:100
    % update x1:
    l=l+1;
    x(2,l)=x(2,l-1);[m1 S1]=GaussCond([nan x(2,l-1)],m,S);  x(1,l) = mvrandn(m1,S1);
    line([x(1,l-1) x(1,l)],[x(2,l-1) x(2,l)])
    
    % update x2:
    l=l+1;
    x(1,l)=x(1,l-1);[m2 S2]=GaussCond([x(1,l-1) nan],m,S);  x(2,l) = mvrandn(m2,S2);
    line([x(1,l-1) x(1,l)],[x(2,l-1) x(2,l)])
end