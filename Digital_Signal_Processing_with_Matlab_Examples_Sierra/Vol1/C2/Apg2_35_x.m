% Example of un-importance sampling 
% the chosen p(x) PDF is not appropriate

% the integrand functions
x=0:0.02:2; %domain of f(x)
xx=0:0.02:8; %domain of p(x)
f=1.8*x-(x.^2); %the function f(x)
p=unifpdf(xx,1,7);%the function p(x) (uniform PDF)

%display of f(x) and p(x)
figure(1)
plot(x+3,f,'k'); hold on; %f() is shifted to the center
plot(xx,p,'r');
axis([0 8 0 1]);
title('Example of un-importance sampling: f(x) and p(x)');
xlabel('x');
