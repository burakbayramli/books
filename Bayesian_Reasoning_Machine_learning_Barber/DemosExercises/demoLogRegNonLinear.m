function demoLogRegNonLinear
%DEMOLOGREGNONLINEAR demo of non-linear regression
figure
% get some training data
x0=0.1*randn(2,3); x0=[x0 (0.1*randn(2,3)+repmat([1 1]',1,3))];
x1=0.1*randn(2,3)+repmat([1 0]',1,3);
x1=[x1 (0.1*randn(2,3)+repmat([0 1]',1,3))];
phi0=LPMquad(x0);
phi1=LPMquad(x1);

opts.maxit=1000; 
opts.tol=0.001;
opts.eta=0.1;
[w b p10 p11]=LogReg(phi0,phi1,opts); % train using Gradient Ascent
% 
% plot the decision boundary
 xx1=[-0.5:0.02:1.5];
 xx2=[-0.5:.02:1.5];
 for i=1:length(xx1);
     for j=1:length(xx2);
         c(i,j)=sigma(b+w'*LPMquad([xx1(i) xx2(j)]'));
     end
 end
 [cs,h]=contour(xx1,xx2,c,[0.1:0.1:0.9]); clabel(cs,h);
 hold on
 plot(x0(1,:),x0(2,:),'r+','markersize',6,'linewidth',3)
 plot(x1(1,:),x1(2,:),'bo','markersize',6,'linewidth',3)