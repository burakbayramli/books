% Laplace random signal
% Histogram

N=10000;
a=rand(1,N); %uniform random data
er=-log(1-a);%exponential random data
lr=er.*sign(rand(1,N)-0.5); %Laplace random data

%display
figure(1)
hist(lr,50);
axis([-10 10 0 2000]);
title('Histogram of Laplace random signal'); 
