function demoARlds
%DEMOARLDS demo of fitting an autoregressive model to data
figure
% generate from an AR LDS
L=3; % number of AR coefficients
T=2000; % number of timepoints
sigma2V=0.001; % output noise
sigma2H=0.0001; % AR coefficient transition noise

% generate some training data:
r=rand; r2=rand;
x=1:T;
for t=1:T
	v(t)=sin(0.5*x(t)*r)+sin(0.5*x(t)*r2)+0.001*randn; 
	r=r+0.001*randn; r2=r2+0.001*randn;
end
subplot(3,1,1); plot(v); title('time series')

% Learn the AR coefficients:
[f g]=ARlds(v,L,sigma2V,sigma2H);

subplot(3,1,2);imagesc(f); title('filtered AR coefficients')
subplot(3,1,3);imagesc(g);title('smoothed AR coefficients')