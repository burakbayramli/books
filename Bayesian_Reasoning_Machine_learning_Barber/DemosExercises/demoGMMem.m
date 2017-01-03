function demoGMMem
%DEMOGMMEM demo of training a Gaussian mixture model using EM
% Make some training data :
l = 0.2; r1 = 0.5;
for r = 1:100
	rad = r1 + rand*l;  theta = rand*2*pi;
	X(1,r) = rad*cos(theta); X(2,r) = rad*sin(theta);
end
% EM training :
opts.plotlik=1;
opts.plotsolution=1;
opts.maxit=50;
opts.minDeterminant=0.0001;
figure
[P,m,S,loglik,phgn]=GMMem(X,10,opts); 